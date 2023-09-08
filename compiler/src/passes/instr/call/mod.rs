/// This module contains the traits and implementations for adding calls to the
/// PRI in MIR bodies.
use std::vec;

use rustc_abi::FieldIdx;
use rustc_middle::{
    mir::{
        BasicBlock, BinOp, Body, Constant, Local, Operand, Place, ProjectionElem, Statement, UnOp,
    },
    ty::{Const, Ty, TyCtxt},
};
use rustc_target::abi::VariantIdx;

pub(super) mod context;

/*
 * Contexts and RuntimeCallAdder.
 * Based on the location and the statement we are going to add runtime calls for,
 * there are some data that are required to be passed to the runtime or used in
 * MIR generation. We place these data in a `Context` and `RuntimeCallAdder`
 * capabilities is determined by this context. For example, if the information
 * for a destination place (left hand side of an assignment) is available in the
 * current context, then `RuntimeCallAdder` will be able to generate basic blocks
 * corresponding to calling the assignment functions in the runtime library.
 */

/*
 * The following traits are meant for definition of features that we expect
 * from `RuntimeCallAdder` for various call adding situations.
 */

/*
 * These wrappers just ensure the semantics for the runtime call adder and
 * prevent interchangeably using them.
 * Note that these types are different from what pri has declared. They are
 * direct aliases for interface clarification but these are separate structures
 * that provide stricter interface rules.
 */
macro_rules! make_local_wrapper {
    ($name:ident) => {
        #[derive(Clone, Copy)]
        pub struct $name(Local);
        impl From<Local> for $name {
            fn from(value: Local) -> Self {
                Self(value)
            }
        }
        impl From<$name> for Local {
            fn from(value: $name) -> Self {
                value.0
            }
        }
    };
}
make_local_wrapper!(PlaceRef);
make_local_wrapper!(OperandRef);

pub(crate) trait PlaceReferencer<'tcx> {
    fn reference_place(&mut self, place: &Place<'tcx>) -> PlaceRef;
}

pub(crate) trait OperandReferencer<'tcx> {
    fn reference_operand(&mut self, operand: &Operand<'tcx>) -> OperandRef;
}

pub(crate) trait Assigner<'tcx> {
    type Cast<'a>: CastAssigner<'tcx>
    where
        Self: 'a;

    fn by_use(&mut self, operand: OperandRef);

    fn by_repeat(&mut self, operand: OperandRef, count: &Const<'tcx>);

    fn by_ref(&mut self, place: PlaceRef, is_mutable: bool);

    fn by_thread_local_ref(&mut self);

    fn by_address_of(&mut self, place: PlaceRef, is_mutable: bool);

    fn by_len(&mut self, place: PlaceRef);

    fn by_cast(&mut self, operand: OperandRef) -> Self::Cast<'_>;

    fn by_binary_op(
        &mut self,
        operator: &BinOp,
        first: OperandRef,
        second: OperandRef,
        checked: bool,
    );

    fn by_unary_op(&mut self, operator: &UnOp, operand: OperandRef);

    fn by_discriminant(&mut self, place: PlaceRef);

    fn by_aggregate_array(&mut self, items: &[OperandRef]);

    fn by_aggregate_tuple(&mut self, fields: &[OperandRef]);

    fn by_aggregate_struct(&mut self, fields: &[OperandRef]);

    fn by_aggregate_enum(&mut self, fields: &[OperandRef], variant: VariantIdx);

    fn by_aggregate_union(&mut self, active_field: FieldIdx, value: OperandRef);

    // Special case for SetDiscriminant StatementType since it is similar to a regular assignment
    fn its_discriminant_to(&mut self, variant_index: &VariantIdx);
}

pub(crate) trait CastAssigner<'tcx> {
    fn to_int(&mut self, ty: Ty<'tcx>);

    fn to_float(&mut self, ty: Ty<'tcx>);

    fn through_unsizing(&mut self);
}

#[derive(Clone, Copy)]
pub struct SwitchInfo<'tcx> {
    pub(super) node_index: BasicBlock,
    pub(super) discr_ty: Ty<'tcx>,
    pub(super) runtime_info_store_var: Local,
}

pub(crate) trait BranchingReferencer<'tcx> {
    fn store_branching_info(&mut self, discr: &Operand<'tcx>) -> SwitchInfo<'tcx>;
}
pub(crate) trait BranchingHandler {
    fn take_by_value(&mut self, value: u128);

    fn take_otherwise<I>(&mut self, non_values: I)
    where
        I: IntoIterator<Item = u128> + ExactSizeIterator,
        I::IntoIter: ExactSizeIterator<Item = u128>;
}

pub trait FunctionHandler<'tcx> {
    fn before_call_func(&mut self, func: OperandRef, arguments: impl Iterator<Item = OperandRef>);

    fn enter_func(&mut self);

    fn return_from_func(&mut self);

    fn after_call_func(&mut self, destination: &Place<'tcx>);
}

pub(crate) trait EntryFunctionHandler {
    fn init_runtime_lib(&mut self);
}

pub(crate) trait AssertionHandler<'tcx> {
    fn check_assert(
        &mut self,
        cond: OperandRef,
        expected: bool,
        msg: &rustc_middle::mir::AssertMessage<'tcx>,
    );

    fn reference_assert_kind(
        &mut self,
        msg: &rustc_middle::mir::AssertMessage<'tcx>,
    ) -> (&'static str, Vec<Operand<'tcx>>, Vec<Statement<'tcx>>);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InsertionLocation {
    Before(BasicBlock),
    After(BasicBlock),
}

impl InsertionLocation {
    #[inline]
    fn index(&self) -> BasicBlock {
        match self {
            Self::Before(bb) | Self::After(bb) => *bb,
        }
    }
}

pub(crate) struct RuntimeCallAdder<C> {
    context: C,
}

mod implementation {
    use std::assert_matches::debug_assert_matches;

    use rustc_middle::mir::{self, BasicBlock, BasicBlockData, Terminator, UnevaluatedConst};
    use rustc_middle::ty::TyKind;
    use rustc_span::def_id::DefId;

    use delegate::delegate;

    use self::ctxtreqs::*;
    use super::context::*;
    use super::*;
    use crate::mir_transform::*;
    use crate::passes::ctfe::CtfeId;
    use crate::passes::instr;
    use crate::passes::Storage;

    use utils::*;
    use InsertionLocation::*;

    pub(crate) trait MirCallAdder<'tcx> {
        fn make_bb_for_call(
            &mut self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
        ) -> BasicBlockData<'tcx> {
            self.make_bb_for_call_with_ret(func_name, args).0
        }

        fn make_bb_for_call_with_target(
            &mut self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
            target: Option<BasicBlock>,
        ) -> BasicBlockData<'tcx> {
            self.make_bb_for_call_with_target_and_ret(func_name, args, target)
                .0
        }

        fn make_bb_for_call_with_ret(
            &mut self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
        ) -> (BasicBlockData<'tcx>, Local) {
            self.make_bb_for_call_with_target_and_ret(func_name, args, None)
        }

        fn make_bb_for_call_with_target_and_ret(
            &mut self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
            target: Option<BasicBlock>,
        ) -> (BasicBlockData<'tcx>, Local);
    }

    pub(crate) trait BlockInserter<'tcx> {
        fn insert_blocks(
            &mut self,
            blocks: impl IntoIterator<Item = BasicBlockData<'tcx>>,
        ) -> Vec<BasicBlock>;

        fn insert_blocks_with_stickiness(
            &mut self,
            blocks: impl IntoIterator<Item = BasicBlockData<'tcx>>,
            sticky: bool,
        ) -> Vec<BasicBlock>;
    }

    impl<'tcx, 'm, 's> RuntimeCallAdder<DefaultContext<'tcx, 'm, 's>> {
        pub fn new(
            tcx: TyCtxt<'tcx>,
            modification_unit: &'m mut BodyInstrumentationUnit<'tcx>,
            storage: &'s mut dyn Storage,
        ) -> Self {
            Self {
                context: DefaultContext::new(tcx, modification_unit, storage),
            }
        }
    }

    /*
     * The following methods are context definers that should be side-effect-free.
     * The reason that self is mutably borrowed is because of the mutability of the
     * context.
     * NOTE: Maybe this is a design mistake. Currently, the only mutable component
     * is the modification unit which may be extracted from the context and stored
     * directly in the RuntimeCallAdder. However, this change should be done when
     * the call adder is quite stable and no substantial change is expected.
     */
    impl<C> RuntimeCallAdder<C> {
        pub fn in_body<'b, 'tcx, 'bd>(
            &'b mut self,
            body: &'bd Body<'tcx>,
        ) -> RuntimeCallAdder<InBodyContext<'b, 'tcx, 'bd, C>> {
            self.with_context(|base| InBodyContext { base, body })
        }

        pub fn at(
            &mut self,
            location: InsertionLocation,
        ) -> RuntimeCallAdder<AtLocationContext<C>> {
            self.with_context(|base| AtLocationContext { base, location })
        }

        pub fn before(&mut self) -> RuntimeCallAdder<AtLocationContext<C>>
        where
            C: BlockIndexProvider,
        {
            let index = self.context.block_index();
            self.with_context(|base| AtLocationContext {
                base,
                location: Before(index),
            })
        }

        pub fn after(&mut self) -> RuntimeCallAdder<AtLocationContext<C>>
        where
            C: BlockIndexProvider,
        {
            let index = self.context.block_index();
            self.with_context(|base| AtLocationContext {
                base,
                location: After(index),
            })
        }

        pub fn assign(&mut self, dest_ref: PlaceRef) -> RuntimeCallAdder<AssignmentContext<C>> {
            self.with_context(|base| AssignmentContext { base, dest_ref })
        }

        pub fn branch<'tcx, 'b>(
            &'b mut self,
            info: SwitchInfo<'tcx>,
        ) -> RuntimeCallAdder<BranchingContext<'b, 'tcx, C>> {
            self.with_context(|base| BranchingContext {
                base,
                switch_info: info,
            })
        }

        pub fn in_entry_fn(&mut self) -> RuntimeCallAdder<EntryFunctionMarkerContext<C>> {
            self.with_context(|base| EntryFunctionMarkerContext { base })
        }

        pub fn borrow_from(
            other: &mut RuntimeCallAdder<C>,
        ) -> RuntimeCallAdder<TransparentContext<C>> {
            other.with_context(|base| TransparentContext { base })
        }

        pub fn with_context<'a: 'b, 'b, NC>(
            &'a mut self,
            f: impl FnOnce(&'b mut C) -> NC,
        ) -> RuntimeCallAdder<NC> {
            RuntimeCallAdder {
                context: f(&mut self.context),
            }
        }
    }

    impl<'tcx, C> RuntimeCallAdder<C>
    where
        C: context::BodyProvider<'tcx> + context::TyContextProvider<'tcx>,
    {
        pub fn current_func(&self) -> rustc_middle::mir::Operand<'tcx> {
            rustc_middle::mir::Operand::function_handle(
                self.context.tcx(),
                self.context.body().source.def_id(),
                ::std::iter::empty(),
                self.context.body().span,
            )
        }
    }

    impl<'tcx, C> TyContextProvider<'tcx> for RuntimeCallAdder<C>
    where
        C: TyContextProvider<'tcx>,
    {
        delegate! {
            to self.context {
                fn tcx(&self) -> TyCtxt<'tcx>;
            }
        }
    }
    impl<'tcx, C> BodyProvider<'tcx> for RuntimeCallAdder<C>
    where
        C: BodyProvider<'tcx>,
    {
        delegate! {
            to self.context {
                fn body(&self) -> &Body<'tcx>;
            }
        }
    }

    impl<'tcx, C> MirCallAdder<'tcx> for RuntimeCallAdder<C>
    where
        C: BodyLocalManager<'tcx> + TyContextProvider<'tcx> + PriItemsProvider<'tcx>,
    {
        fn make_bb_for_call_with_target_and_ret(
            &mut self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
            target: Option<BasicBlock>,
        ) -> (BasicBlockData<'tcx>, Local) {
            let result_local = self
                .context
                .add_local(self.context.get_pri_func_info(func_name).ret_ty);
            (
                self.make_call_bb(func_name, args, Place::from(result_local), target),
                result_local,
            )
        }
    }
    impl<'tcx, C> RuntimeCallAdder<C>
    where
        C: TyContextProvider<'tcx> + PriItemsProvider<'tcx>,
    {
        fn make_call_bb(
            &self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
            destination: Place<'tcx>,
            target: Option<BasicBlock>,
        ) -> BasicBlockData<'tcx> {
            BasicBlockData::new(Some(self.make_call_terminator(
                func_name,
                args,
                destination,
                target,
            )))
        }

        fn make_call_terminator(
            &self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
            destination: Place<'tcx>,
            target: Option<BasicBlock>,
        ) -> Terminator<'tcx> {
            terminator::call(
                self.context.tcx(),
                self.context.get_pri_func_info(func_name).def_id,
                args,
                destination,
                target,
            )
        }
    }

    impl<'tcx, C> BlockInserter<'tcx> for RuntimeCallAdder<C>
    where
        C: ForInsertion<'tcx>,
    {
        #[inline]
        fn insert_blocks(
            &mut self,
            blocks: impl IntoIterator<Item = BasicBlockData<'tcx>>,
        ) -> Vec<BasicBlock> {
            match self.context.insertion_loc() {
                Before(_) => self.insert_blocks_with_stickiness(blocks, true),
                After(index) => self.context.insert_blocks_after(index, blocks),
            }
        }

        fn insert_blocks_with_stickiness(
            &mut self,
            blocks: impl IntoIterator<Item = BasicBlockData<'tcx>>,
            sticky: bool,
        ) -> Vec<BasicBlock> {
            debug_assert_matches!(
                self.context.insertion_loc(),
                Before(..),
                "Stickiness is only defined for insertions before a block."
            );
            self.context
                .insert_blocks_before(self.context.block_index(), blocks, sticky)
        }
    }

    impl<'tcx, C> PlaceReferencer<'tcx> for RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: ForPlaceRef<'tcx>,
    {
        fn reference_place(&mut self, place: &Place<'tcx>) -> PlaceRef {
            let BlocksAndResult(new_blocks, reference) = self.internal_reference_place(place);
            self.insert_blocks(new_blocks);
            reference.into()
        }
    }
    impl<'tcx, C> RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: ForPlaceRef<'tcx>,
    {
        fn internal_reference_place(&mut self, place: &Place<'tcx>) -> BlocksAndResult<'tcx> {
            let BlocksAndResult(mut blocks, place_ref) = self.reference_place_local(place.local);

            let tcx = self.tcx();

            // For setting addresses we have to remake a cumulative place up to each projection.
            let mut cum_place = Place::from(place.local);
            let mut cum_ty = cum_place.ty(&self.context, tcx);
            if cfg!(place_addr) {
                blocks.push(self.make_bb_for_set_addr_call(place_ref, &cum_place, cum_ty.ty));
            }

            for (_, proj) in place.iter_projections() {
                let added_blocks = self.reference_place_projection(place_ref, proj);
                blocks.extend(added_blocks);

                if cfg!(place_addr) {
                    cum_place = cum_place.project_deeper(&[proj], tcx);
                    cum_ty = cum_ty.projection_ty(tcx, proj);
                    blocks.push(self.make_bb_for_set_addr_call(place_ref, &cum_place, cum_ty.ty));
                }
            }

            if cfg!(place_addr) {
                if let Some(block) = self.make_bb_for_set_type_call(place_ref, cum_ty.ty) {
                    blocks.push(block);
                }
            }

            BlocksAndResult(blocks, place_ref)
        }

        fn reference_place_local(&mut self, local: Local) -> BlocksAndResult<'tcx> {
            let func_name = if local == mir::RETURN_PLACE {
                stringify!(pri::ref_place_return_value)
            } else if local.as_usize() <= self.context.body().arg_count {
                stringify!(pri::ref_place_argument)
            } else {
                stringify!(pri::ref_place_local)
            };
            let args = if local == mir::RETURN_PLACE {
                vec![]
            } else {
                vec![operand::const_from_uint(self.context.tcx(), local.as_u32())]
            };

            self.make_bb_for_place_ref_call(func_name, args).into()
        }

        fn reference_place_projection<T>(
            &mut self,
            current_ref: Local,
            proj: ProjectionElem<Local, T>,
        ) -> Vec<BasicBlockData<'tcx>> {
            let mut new_blocks = Vec::new();

            let (func_name, additional_args) = match proj {
                ProjectionElem::Deref => (stringify!(pri::ref_place_deref), vec![]),
                ProjectionElem::Field(index, _) => (
                    stringify!(pri::ref_place_field),
                    vec![operand::const_from_uint(
                        self.context.tcx(),
                        u32::from(index),
                    )],
                ),
                ProjectionElem::Index(index) => {
                    let BlocksAndResult(additional_blocks, index_ref) =
                        self.internal_reference_place(&Place::from(index));
                    new_blocks.extend(additional_blocks);
                    (
                        stringify!(pri::ref_place_index),
                        vec![operand::copy_for_local(index_ref)],
                    )
                }
                ProjectionElem::ConstantIndex {
                    offset,
                    min_length,
                    from_end,
                } => (
                    stringify!(pri::ref_place_constant_index),
                    vec![
                        operand::const_from_uint(self.context.tcx(), offset),
                        operand::const_from_uint(self.context.tcx(), min_length),
                        operand::const_from_bool(self.context.tcx(), from_end),
                    ],
                ),
                ProjectionElem::Subslice { from, to, from_end } => (
                    stringify!(pri::ref_place_subslice),
                    vec![
                        operand::const_from_uint(self.context.tcx(), from),
                        operand::const_from_uint(self.context.tcx(), to),
                        operand::const_from_bool(self.context.tcx(), from_end),
                    ],
                ),
                ProjectionElem::Downcast(_, index) => (
                    stringify!(pri::ref_place_downcast),
                    vec![operand::const_from_uint(
                        self.context.tcx(),
                        u32::from(index),
                    )],
                ),
                ProjectionElem::OpaqueCast(_) => (stringify!(pri::ref_place_opaque_cast), vec![]),
            };

            new_blocks.push(
                self.make_bb_for_place_ref_call(
                    func_name,
                    [vec![operand::copy_for_local(current_ref)], additional_args].concat(),
                )
                .0, // The result is unit.
            );
            new_blocks
        }

        fn make_bb_for_place_ref_call(
            &mut self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
        ) -> (BasicBlockData<'tcx>, Local) {
            self.make_bb_for_call_with_ret(func_name, args)
        }

        fn make_bb_for_set_addr_call(
            &mut self,
            place_ref: Local,
            place: &Place<'tcx>,
            place_ty: Ty<'tcx>,
        ) -> BasicBlockData<'tcx> {
            let (statements, addr_local) = self.add_and_set_local_for_ptr(place, place_ty);

            let mut block = self.make_bb_for_call(
                stringify!(pri::set_place_address),
                vec![
                    operand::copy_for_local(place_ref),
                    operand::move_for_local(addr_local),
                ],
            );

            block.statements.extend(statements);
            block
        }

        /// Makes a basic block for setting the type of a place if it is from
        /// primitive types.
        fn make_bb_for_set_type_call(
            &mut self,
            place_ref: Local,
            ty: Ty<'tcx>,
        ) -> Option<BasicBlockData<'tcx>> {
            let tcx = self.context.tcx();
            let (func_name, additional_args) = if ty.is_bool() {
                (stringify!(pri::set_place_type_bool), vec![])
            } else if ty.is_char() {
                (stringify!(pri::set_place_type_char), vec![])
            } else if ty.is_integral() {
                (
                    stringify!(pri::set_place_type_int),
                    vec![
                        operand::const_from_uint(tcx, ty::size_of(self.tcx(), ty).bits()),
                        operand::const_from_bool(tcx, ty.is_signed()),
                    ],
                )
            } else if ty.is_floating_point() {
                let (e_bits, s_bits) = ty::ebit_sbit_size(tcx, ty);
                (
                    stringify!(pri::set_place_type_float),
                    vec![
                        operand::const_from_uint(tcx, e_bits),
                        operand::const_from_uint(tcx, s_bits),
                    ],
                )
            } else {
                return None;
            };

            Some(self.make_bb_for_call(
                func_name,
                [vec![operand::copy_for_local(place_ref)], additional_args].concat(),
            ))
        }

        fn add_and_set_local_for_ptr(
            &mut self,
            place: &Place<'tcx>,
            place_ty: Ty<'tcx>,
        ) -> ([Statement<'tcx>; 2], Local) {
            let tcx = self.tcx();
            let ptr_local = self.context.add_local(tcx.mk_imm_ptr(place_ty));
            let ptr_place = Place::from(ptr_local);
            let ptr_assignment = assignment::create(
                ptr_place,
                mir::Rvalue::AddressOf(rustc_ast::Mutability::Not, place.clone()),
            );
            let pri_ptr_ty = self.context.pri_types().raw_ptr;
            let cast_local = self.context.add_local(pri_ptr_ty);
            let cast_assignment = assignment::create(
                cast_local.into(),
                rvalue::cast_expose_addr(Operand::Move(ptr_place), pri_ptr_ty),
            );

            ([ptr_assignment, cast_assignment], cast_local)
        }
    }

    impl<'tcx, C> OperandReferencer<'tcx> for RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: ForOperandRef<'tcx>,
    {
        fn reference_operand(&mut self, operand: &Operand<'tcx>) -> OperandRef {
            let BlocksAndResult(new_blocks, reference) = self.internal_reference_operand(operand);
            self.insert_blocks(new_blocks);
            reference.into()
        }
    }
    #[allow(clippy::borrowed_box)]
    impl<'tcx, C> RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: ForOperandRef<'tcx>,
    {
        fn internal_reference_operand(&mut self, operand: &Operand<'tcx>) -> BlocksAndResult<'tcx> {
            match operand {
                Operand::Copy(place) | Operand::Move(place) => {
                    let BlocksAndResult(additional_blocks, place_ref) =
                        self.internal_reference_place(place);

                    let func_name = if let Operand::Copy(_) = operand {
                        stringify!(pri::ref_operand_copy)
                    } else {
                        stringify!(pri::ref_operand_move)
                    };

                    BlocksAndResult::from(self.make_bb_for_operand_ref_call(
                        func_name,
                        vec![operand::copy_for_local(place_ref)],
                    ))
                    .prepend(additional_blocks)
                }
                Operand::Constant(constant) => self.internal_reference_const_operand(constant),
            }
        }

        fn internal_reference_const_operand(
            &mut self,
            constant: &Box<Constant<'tcx>>,
        ) -> BlocksAndResult<'tcx> {
            /* NOTE: Although there might be some ways to obtain/evaluate the values, we prefer to use
             * the constant as it is, and rely on high-level conversions to support all types of
             * constants in an abstract fashion. */
            let ty = constant.ty();
            let tcx = self.tcx();
            if ty.is_bool() {
                self.internal_reference_const_operand_directly(
                    stringify!(pri::ref_operand_const_bool),
                    constant,
                )
            } else if ty.is_integral() {
                self.internal_reference_int_const_operand(constant)
            } else if ty.is_floating_point() {
                self.internal_reference_float_const_operand(constant)
            } else if ty.is_char() {
                self.internal_reference_const_operand_directly(
                    stringify!(pri::ref_operand_const_char),
                    constant,
                )
            } else if ty.peel_refs().is_str() {
                self.internal_reference_const_operand_directly(
                    stringify!(pri::ref_operand_const_str),
                    constant,
                )
            }
            // &[u8]
            else if Self::is_u8_slice_ref(tcx, ty) {
                self.internal_reference_const_operand_directly(
                    stringify!(pri::ref_operand_const_byte_str),
                    constant,
                )
            }
            // &[u8; N]
            else if ty.peel_refs().is_array()
                && ty.peel_refs().sequence_element_type(tcx) == tcx.types.u8
            {
                self.internal_reference_byte_str_const_operand(constant)
            } else if let TyKind::FnDef(def_id, substs) = ty.kind() {
                self.internal_reference_func_def_const_operand(def_id, substs)
            }
            // NOTE: Check this after all other ZSTs that you want to distinguish.
            else if ty.is_trivially_sized(tcx) && ty::size_of(tcx, ty) == rustc_abi::Size::ZERO {
                self.make_bb_for_operand_ref_call(
                    stringify!(pri::ref_operand_const_zst),
                    Default::default(),
                )
                .into()
            } else if let Some(c) = operand::const_try_as_unevaluated(constant) {
                self.internal_reference_unevaluated_const_operand(&c)
            } else if let Some(def_id) = Self::try_as_immut_static(tcx, constant) {
                self.internal_reference_static_ref_const_operand(def_id, ty)
            } else {
                unimplemented!(
                    "Unsupported constant: {:?} with type: {:?}",
                    constant.literal,
                    ty
                )
            }
        }

        fn internal_reference_const_operand_directly(
            &mut self,
            func_name: &str,
            constant: &Box<Constant<'tcx>>,
        ) -> BlocksAndResult<'tcx> {
            self.make_bb_for_operand_ref_call(
                func_name,
                vec![operand::const_from_existing(constant)],
            )
            .into()
        }

        fn internal_reference_int_const_operand(
            &mut self,
            constant: &Box<Constant<'tcx>>,
        ) -> BlocksAndResult<'tcx> {
            let ty = constant.ty();
            debug_assert!(ty.is_integral());

            let (bit_rep_local, additional_stmts) =
                utils::cast_int_to_bit_rep(self.tcx(), &mut self.context, constant);
            let (mut block, result) = self.make_bb_for_operand_ref_call(
                stringify!(pri::ref_operand_const_int),
                vec![
                    operand::move_for_local(bit_rep_local),
                    operand::const_from_uint(
                        self.context.tcx(),
                        ty::size_of(self.tcx(), ty).bits(),
                    ),
                    operand::const_from_bool(self.context.tcx(), ty.is_signed()),
                ],
            );
            block.statements.extend(additional_stmts);
            (block, result).into()
        }

        fn internal_reference_float_const_operand(
            &mut self,
            constant: &Box<Constant<'tcx>>,
        ) -> BlocksAndResult<'tcx> {
            let ty = constant.ty();
            debug_assert!(ty.is_floating_point());

            let tcx = self.tcx();
            let (bit_rep_local, conversion_block) =
                utils::cast_float_to_bit_rep(tcx, &mut self.context, constant);
            let (e_bits, s_bits) = ty::ebit_sbit_size(tcx, ty);
            let block_pair = self.make_bb_for_operand_ref_call(
                stringify!(pri::ref_operand_const_float),
                vec![
                    operand::move_for_local(bit_rep_local),
                    operand::const_from_uint(tcx, e_bits),
                    operand::const_from_uint(tcx, s_bits),
                ],
            );
            BlocksAndResult::from(block_pair).prepend([conversion_block])
        }

        fn internal_reference_byte_str_const_operand(
            &mut self,
            constant: &Box<Constant<'tcx>>,
        ) -> BlocksAndResult<'tcx> {
            let tcx = self.tcx();
            let ty = constant.ty();
            debug_assert!(ty.is_ref() && ty.peel_refs().sequence_element_type(tcx) == tcx.types.u8,);

            let (slice_local, slice_assignment) = cast_array_ref_to_slice(
                tcx,
                &mut self.context,
                operand::const_from_existing(constant),
                None,
            );
            let mut block_pair = self.make_bb_for_operand_ref_call(
                stringify!(pri::ref_operand_const_byte_str),
                vec![operand::move_for_local(slice_local)],
            );
            block_pair.0.statements.insert(0, slice_assignment);
            BlocksAndResult::from(block_pair)
        }

        fn internal_reference_func_def_const_operand(
            &mut self,
            def_id: &DefId,
            substs: &[rustc_middle::ty::GenericArg],
        ) -> BlocksAndResult<'tcx> {
            if !substs.is_empty() {
                log::warn!("Referencing a function with substitution variables (generic).");
            }

            /* NOTE: Until we find a better way to represent a function we use  the def id. */
            let func_id: u64 =
                ((u32::from(def_id.krate) as u64) << 32) + (u32::from(def_id.index) as u64);
            self.make_bb_for_operand_ref_call(
                stringify!(pri::ref_operand_const_func),
                vec![operand::const_from_uint(self.context.tcx(), func_id)],
            )
            .into()
        }

        /// ## Remarks
        /// In this case, instead of referencing the constant itself,
        /// we pretend that it is replaced with calling the CTFE block, then
        /// reference the result of calling the corresponding NCTFE.
        /// Note that it is only about the instrumentation and the real
        /// block remains intact.
        /// For example, for the following assignment:
        /// ```mir
        /// _1 = const XYZ;
        /// ```
        /// will be instrumented as:
        /// ```mir
        /// // related instrumentations like before_call
        /// _10 = nctfe_of_XYZ();
        /// // related instrumentations like after_call
        /// _11 = reference(10);
        /// assign(_1, _11);
        /// ```
        fn internal_reference_unevaluated_const_operand(
            &mut self,
            constant: &UnevaluatedConst,
        ) -> BlocksAndResult<'tcx> {
            let def_id = constant.def.expect_local();
            let ctfe_id = if let Some(index) = constant.promoted {
                CtfeId::Promoted(def_id, index)
            } else {
                CtfeId::Const(def_id)
            };

            let nctfe_result_local = self.call_nctfe(ctfe_id);
            self.internal_reference_operand(&operand::move_for_local(nctfe_result_local))
        }

        fn internal_reference_static_ref_const_operand(
            &mut self,
            def_id: DefId,
            ty: Ty<'tcx>,
        ) -> BlocksAndResult<'tcx> {
            let item_ty = match ty.kind() {
                TyKind::Ref(_, ty, rustc_ast::Mutability::Not) => ty.clone(),
                _ => unreachable!("Constant type should be an immutable reference."),
            };

            let tcx = self.tcx();
            let ctfe_id = CtfeId::Const(def_id.expect_local());

            assert!(
                ty::is_ref_assignable(&item_ty, &ctfe_id.ty(tcx)),
                concat!(
                    "Constant type should be the reference of the init block's return type.",
                    "Constant type: {:?}, init block return type: {:?}",
                ),
                ty,
                ctfe_id.ty(tcx)
            );

            let nctfe_result_local = self.call_nctfe(ctfe_id);

            // Simulate referencing the result of the NCTFE call.
            let ref_local = self.context.add_local(ty);
            {
                // FIXME: Duplicate code with `LeafStatementVisitor::visit_assign`.
                let ref_local_ref = self.reference_place(&ref_local.into());
                instr::LeafAssignmentVisitor::instrument_ref(
                    &mut self.assign(ref_local_ref),
                    &rustc_middle::mir::BorrowKind::Shared,
                    &nctfe_result_local.into(),
                );
            }

            self.internal_reference_operand(&operand::move_for_local(ref_local))
        }

        fn call_nctfe(&mut self, id: CtfeId) -> Local {
            use crate::passes::ctfe::get_nctfe_func;

            let tcx = self.tcx();
            let result_local = self.context.add_local(id.ty(tcx));

            let nctfe_id = get_nctfe_func(id, self.context.storage());
            let call_block = BasicBlockData::new(Some(terminator::call(
                tcx,
                nctfe_id,
                Vec::default(),
                result_local.into(),
                None,
            )));
            let call_block_index = self.insert_blocks([call_block])[0];

            instr::LeafTerminatorKindVisitor::instrument_call(
                &mut self.at(Before(call_block_index)),
                |call_adder| {
                    // NOTE: You cannot call reference operand here because of the infinite loop in types.
                    let BlocksAndResult(blocks, func_ref) =
                        call_adder.internal_reference_func_def_const_operand(&nctfe_id, &[]);
                    call_adder.insert_blocks(blocks);
                    func_ref.into()
                },
                |_| Vec::default(),
                &result_local.into(),
                &Some(NEXT_BLOCK),
            );

            result_local
        }

        fn make_bb_for_operand_ref_call(
            &mut self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
        ) -> (BasicBlockData<'tcx>, Local) {
            self.make_bb_for_call_with_ret(func_name, args)
        }

        #[inline]
        fn is_u8_slice_ref(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> bool {
            // This is just for mitigating the bug in rustfmt. Track: rustfmt#5863
            if let TyKind::Ref(_, ty, _) = ty.kind() {
                if let TyKind::Slice(ty) = ty.kind() {
                    return ty == &tcx.types.u8;
                }
            }

            false
        }

        #[inline]
        fn try_as_immut_static(tcx: TyCtxt<'tcx>, constant: &Box<Constant<'tcx>>) -> Option<DefId> {
            /* Immutable statics are accessed by a constant reference which points to a statically
             * allocated program memory block. If the static item's type is T then the constant is
             * of type &T. */
            constant
                .ty()
                .ref_mutability()
                .is_some_and(|m| m.is_not())
                .then(|| operand::const_try_as_ptr(constant))
                .flatten()
                .and_then(|alloc_id| match tcx.global_alloc(alloc_id) {
                    rustc_middle::mir::interpret::GlobalAlloc::Static(id) => Some(id),
                    _ => None,
                })
        }
    }

    impl<'tcx, C> Assigner<'tcx> for RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: ForAssignment<'tcx>,
    {
        type Cast<'b> = RuntimeCallAdder<CastAssignmentContext<'b, C>> where C: 'b;

        fn by_use(&mut self, operand: OperandRef) {
            self.add_bb_for_assign_call(
                stringify!(pri::assign_use),
                vec![operand::copy_for_local(operand.into())],
            )
        }

        fn by_repeat(&mut self, operand: OperandRef, count: &Const<'tcx>) {
            debug_assert_eq!(count.ty(), self.tcx().types.usize);
            self.add_bb_for_assign_call(
                stringify!(pri::assign_repeat),
                vec![
                    operand::copy_for_local(operand.into()),
                    #[allow(clippy::clone_on_copy)]
                    operand::const_from_existing_ty_const(count.clone()),
                ],
            )
        }

        fn by_ref(&mut self, place: PlaceRef, is_mutable: bool) {
            self.add_bb_for_assign_call(
                stringify!(pri::assign_ref),
                vec![
                    operand::copy_for_local(place.into()),
                    operand::const_from_bool(self.context.tcx(), is_mutable),
                ],
            )
        }

        fn by_thread_local_ref(&mut self) {
            todo!()
        }

        fn by_address_of(&mut self, place: PlaceRef, is_mutable: bool) {
            self.add_bb_for_assign_call(
                stringify!(pri::assign_address_of),
                vec![
                    operand::copy_for_local(place.into()),
                    operand::const_from_bool(self.context.tcx(), is_mutable),
                ],
            )
        }

        fn by_len(&mut self, place: PlaceRef) {
            self.add_bb_for_assign_call(
                stringify!(pri::assign_len),
                vec![operand::copy_for_local(place.into())],
            )
        }

        fn by_cast(&mut self, operand: OperandRef) -> Self::Cast<'_> {
            RuntimeCallAdder {
                context: CastAssignmentContext {
                    base: &mut self.context,
                    operand_ref: operand,
                },
            }
        }

        fn by_binary_op(
            &mut self,
            operator: &BinOp,
            first: OperandRef,
            second: OperandRef,
            checked: bool,
        ) {
            let operator = convert_mir_binop_to_pri(operator);
            let (operator_local, additional_stmts) = self
                .add_and_set_local_for_enum(self.context.pri_types().binary_op, operator as u128);

            self.add_bb_for_assign_call_with_statements(
                stringify!(pri::assign_binary_op),
                vec![
                    operand::move_for_local(operator_local),
                    operand::copy_for_local(first.into()),
                    operand::copy_for_local(second.into()),
                    operand::const_from_bool(self.context.tcx(), checked),
                ],
                additional_stmts,
            )
        }

        fn by_unary_op(&mut self, operator: &UnOp, operand: OperandRef) {
            let operator = convert_mir_unop_to_pri(operator);
            let (operator_local, additional_stmts) = self
                .add_and_set_local_for_enum(self.context.pri_types().unary_op, operator as u128);

            self.add_bb_for_assign_call_with_statements(
                stringify!(pri::assign_unary_op),
                vec![
                    operand::move_for_local(operator_local),
                    operand::copy_for_local(operand.into()),
                ],
                additional_stmts,
            )
        }

        fn by_discriminant(&mut self, place: PlaceRef) {
            self.add_bb_for_assign_call(
                stringify!(pri::assign_discriminant),
                vec![operand::copy_for_local(place.into())],
            )
        }

        fn by_aggregate_array(&mut self, items: &[OperandRef]) {
            self.add_bb_for_aggregate_assign_call(
                stringify!(pri::assign_aggregate_array),
                items,
                vec![],
            )
        }

        fn by_aggregate_tuple(&mut self, fields: &[OperandRef]) {
            self.add_bb_for_aggregate_assign_call(
                stringify!(pri::assign_aggregate_tuple),
                fields,
                vec![],
            )
        }

        fn by_aggregate_struct(&mut self, fields: &[OperandRef]) {
            self.add_bb_for_aggregate_assign_call(
                stringify!(pri::assign_aggregate_struct),
                fields,
                vec![],
            )
        }

        fn by_aggregate_enum(&mut self, fields: &[OperandRef], variant: VariantIdx) {
            self.add_bb_for_aggregate_assign_call(
                stringify!(pri::assign_aggregate_enum),
                fields,
                vec![operand::const_from_uint(
                    self.context.tcx(),
                    variant.as_u32(),
                )],
            )
        }

        fn by_aggregate_union(&mut self, active_field: FieldIdx, value: OperandRef) {
            self.add_bb_for_assign_call(
                stringify!(pri::assign_aggregate_union),
                vec![
                    operand::const_from_uint(self.context.tcx(), active_field.as_u32()),
                    operand::copy_for_local(value.into()),
                ],
            )
        }

        fn its_discriminant_to(&mut self, variant_index: &VariantIdx) {
            self.add_bb_for_assign_call(
                stringify!(pri::set_discriminant),
                vec![operand::const_from_uint(
                    self.context.tcx(),
                    variant_index.as_u32(),
                )],
            )
        }
    }

    impl<'tcx, C> RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: ForAssignment<'tcx>,
    {
        fn add_bb_for_aggregate_assign_call(
            &mut self,
            func_name: &str,
            elements: &[OperandRef],
            additional_args: Vec<Operand<'tcx>>,
        ) {
            let (elements_local, additional_stmts) = self.make_slice_for_adt_elements(elements);

            self.add_bb_for_assign_call_with_statements(
                func_name,
                [
                    vec![operand::move_for_local(elements_local)],
                    additional_args,
                ]
                .concat(),
                additional_stmts.to_vec(),
            )
        }

        fn make_slice_for_adt_elements(
            &mut self,
            elements: &[OperandRef],
        ) -> (Local, [Statement<'tcx>; 3]) {
            let operand_ref_ty = self.context.pri_types().operand_ref;
            let (items_local, additional_stmts) = prepare_operand_for_slice(
                self.context.tcx(),
                &mut self.context,
                operand_ref_ty,
                elements
                    .iter()
                    .map(|i| operand::move_for_local((*i).into()))
                    .collect(),
            );
            (items_local, additional_stmts)
        }

        fn add_bb_for_assign_call(&mut self, func_name: &str, args: Vec<Operand<'tcx>>) {
            self.add_bb_for_assign_call_with_statements(func_name, args, vec![])
        }

        fn add_bb_for_assign_call_with_statements(
            &mut self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
            statements: Vec<Statement<'tcx>>,
        ) {
            let mut block = self.make_bb_for_assign_call(func_name, args);
            block.statements.extend(statements);
            self.insert_blocks([block]);
        }

        fn make_bb_for_assign_call(
            &mut self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
        ) -> BasicBlockData<'tcx> {
            self.make_bb_for_call(
                func_name,
                [
                    vec![operand::copy_for_local(self.context.dest_ref().into())],
                    args,
                ]
                .concat(),
            )
        }

        fn add_and_set_local_for_enum(
            &mut self,
            enum_ty: Ty<'tcx>,
            discr: impl Into<u128>,
        ) -> (Local, Vec<Statement<'tcx>>) {
            let local = self.context.add_local(enum_ty);
            let statements = enums::set_variant_to_local(self.tcx(), enum_ty, discr.into(), local);
            (local, statements)
        }
    }

    impl<'tcx, C> CastAssigner<'tcx> for RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: CastOperandProvider + ForAssignment<'tcx>,
    {
        fn to_int(&mut self, ty: Ty<'tcx>) {
            if ty.is_char() {
                self.add_bb_for_cast_assign_call(stringify!(pri::assign_cast_char))
            } else {
                assert!(ty.is_integral());

                let tcx = self.context.tcx();
                let is_signed = ty.is_signed();
                let bits = ty::size_of(tcx, ty).bits();

                self.add_bb_for_cast_assign_call_with_args(
                    stringify!(pri::assign_cast_integer),
                    vec![
                        operand::const_from_uint(tcx, bits),
                        operand::const_from_bool(tcx, is_signed),
                    ],
                )
            }
        }

        fn to_float(&mut self, ty: Ty<'tcx>) {
            let (e_bits, s_bits) = ty::ebit_sbit_size(self.context.tcx(), ty);
            self.add_bb_for_cast_assign_call_with_args(
                stringify!(pri::assign_cast_float),
                vec![
                    operand::const_from_uint(self.context.tcx(), e_bits),
                    operand::const_from_uint(self.context.tcx(), s_bits),
                ],
            )
        }

        fn through_unsizing(&mut self) {
            self.add_bb_for_cast_assign_call(stringify!(pri::assign_cast_unsize))
        }
    }

    impl<'tcx, C> RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: CastOperandProvider + ForAssignment<'tcx>,
    {
        fn add_bb_for_cast_assign_call(&mut self, func_name: &str) {
            self.add_bb_for_cast_assign_call_with_args(func_name, vec![])
        }

        fn add_bb_for_cast_assign_call_with_args(
            &mut self,
            func_name: &str,
            args: Vec<Operand<'tcx>>,
        ) {
            self.add_bb_for_assign_call(
                func_name,
                [
                    vec![operand::copy_for_local(self.context.operand_ref().into())],
                    args,
                ]
                .concat(),
            )
        }
    }

    impl<'tcx, C> BranchingReferencer<'tcx> for RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx> + OperandReferencer<'tcx>,
        C: ForBranching<'tcx>,
    {
        fn store_branching_info(&mut self, discr: &Operand<'tcx>) -> SwitchInfo<'tcx> {
            let tcx = self.context.tcx();
            let operand_ref = self.reference_operand(discr);
            let ty = discr.ty(self.context.local_decls(), tcx);
            let discr_size = ty::size_of(tcx, ty).bits();
            let node_index = self.context.block_index();
            let (block, info_store_var) = self.make_bb_for_call_with_ret(
                stringify!(pri::BranchingInfo::new),
                vec![
                    operand::const_from_uint(tcx, u32::from(node_index)),
                    operand::copy_for_local(operand_ref.into()),
                    operand::const_from_uint(tcx, discr_size),
                    operand::const_from_bool(tcx, ty.is_signed()),
                ],
            );
            self.insert_blocks([block]);
            SwitchInfo {
                node_index,
                discr_ty: ty,
                runtime_info_store_var: info_store_var,
            }
        }
    }

    impl<'tcx, C> BranchingHandler for RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: SwitchInfoProvider<'tcx> + ForBranching<'tcx>,
    {
        fn take_by_value(&mut self, value: u128) {
            let switch_info = self.context.switch_info();
            let discr_ty = switch_info.discr_ty;
            let (func_name, additional_args) = if discr_ty.is_bool() {
                const FALSE_SWITCH_VALUE: u128 = 0;
                if value == FALSE_SWITCH_VALUE {
                    (stringify!(pri::take_branch_false), vec![])
                } else {
                    unreachable!(
                        "SwitchInts for booleans are expected to provide only the value 0 (false)."
                    )
                }
            } else if discr_ty.is_integral() {
                // TODO: Distinguish enum discriminant
                (
                    stringify!(pri::take_branch_int),
                    vec![operand::const_from_uint(self.context.tcx(), value)],
                )
            } else if discr_ty.is_char() {
                (
                    stringify!(pri::take_branch_char),
                    vec![operand::const_from_char(
                        self.context.tcx(),
                        char::from_u32(value.try_into().unwrap()).unwrap(),
                    )],
                )
            } else {
                unreachable!(
                    "Branching node discriminant is supposed to be either bool, int, char, or enum discriminant."
                )
            };

            let block = self.make_bb_for_call_with_target(
                func_name,
                [
                    vec![operand::move_for_local(switch_info.runtime_info_store_var)],
                    additional_args,
                ]
                .concat(),
                Some(self.context.block_index()),
            );
            let new_block_index = self.insert_blocks_with_stickiness([block], false)[0];
            self.context.modify_jump_target_where(
                switch_info.node_index,
                self.context.block_index(),
                new_block_index,
                JumpModificationConstraint::SwitchValue(value),
            );
        }

        fn take_otherwise<I>(&mut self, non_values: I)
        where
            I: IntoIterator<Item = u128>,
            I::IntoIter: ExactSizeIterator<Item = u128>,
        {
            let switch_info = self.context.switch_info();
            let discr_ty = switch_info.discr_ty;
            let (additional_stmts, func_name, additional_args) = if discr_ty.is_bool() {
                let mut non_values = non_values.into_iter();
                debug_assert!(non_values.len() == 1);
                debug_assert!(non_values.next().unwrap() == 0);

                (vec![], stringify!(pri::take_branch_true), vec![])
            } else {
                let tcx = self.context.tcx();

                let (func_name, value_type, value_to_operand): (
                    &str,
                    Ty<'tcx>,
                    Box<dyn Fn(u128) -> Operand<'tcx>>,
                ) = if discr_ty.is_integral() {
                    // TODO: Distinguish enum discriminant
                    (
                        stringify!(pri::take_branch_ow_int),
                        tcx.types.u128,
                        Box::new(|nv: u128| operand::const_from_uint(tcx, nv)),
                    )
                } else if discr_ty.is_char() {
                    (
                        stringify!(pri::take_branch_ow_char),
                        tcx.types.char,
                        Box::new(|nv: u128| {
                            operand::const_from_char(
                                tcx,
                                char::from_u32(nv.try_into().unwrap()).unwrap(),
                            )
                        }),
                    )
                } else {
                    unreachable!(
                        "Branching node discriminant is supposed to be either bool, int, char, or enum discriminant."
                    )
                };

                let (non_values_local, assign_stmts) = self.add_and_assign_local_for_ow_non_values(
                    non_values.into_iter(),
                    value_type,
                    value_to_operand,
                );
                (
                    assign_stmts.to_vec(),
                    func_name,
                    vec![operand::move_for_local(non_values_local)],
                )
            };

            let mut block = self.make_bb_for_call_with_target(
                func_name,
                [
                    vec![operand::move_for_local(switch_info.runtime_info_store_var)],
                    additional_args,
                ]
                .concat(),
                Some(self.context.block_index()),
            );
            block.statements.extend(additional_stmts);
            let new_block_index = self.insert_blocks_with_stickiness([block], false)[0];
            self.context.modify_jump_target_where(
                switch_info.node_index,
                self.context.block_index(),
                new_block_index,
                JumpModificationConstraint::SwitchOtherwise,
            );
        }
    }
    impl<'tcx, C> RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: SwitchInfoProvider<'tcx> + ForBranching<'tcx>,
    {
        fn add_and_assign_local_for_ow_non_values(
            &mut self,
            non_values: impl ExactSizeIterator<Item = u128>,
            value_ty: Ty<'tcx>,
            value_to_operand: impl Fn(u128) -> Operand<'tcx>,
        ) -> (Local, [Statement<'tcx>; 3]) {
            prepare_operand_for_slice(
                self.context.tcx(),
                &mut self.context,
                value_ty,
                non_values.map(value_to_operand).collect(),
            )
        }
    }

    impl<'tcx, C> FunctionHandler<'tcx> for RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: ForFunctionCalling<'tcx>,
    {
        fn before_call_func(
            &mut self,
            func: OperandRef,
            arguments: impl Iterator<Item = OperandRef>,
        ) {
            let operand_ref_ty = self.context.pri_types().operand_ref;
            let (arguments_local, additional_stmts) = prepare_operand_for_slice(
                self.context.tcx(),
                &mut self.context,
                operand_ref_ty,
                arguments
                    .map(|a| operand::copy_for_local(a.into()))
                    .collect(),
            );
            let mut block = self.make_bb_for_call(
                stringify!(pri::before_call_func),
                vec![
                    operand::copy_for_local(func.into()),
                    operand::move_for_local(arguments_local),
                ],
            );
            block.statements.extend(additional_stmts);
            debug_assert_matches!(
                self.context.insertion_loc(),
                Before(..),
                "Inserting before_call after a block is not expected."
            );
            self.insert_blocks([block]);
        }

        fn enter_func(&mut self) {
            let func = self.current_func();
            let func_ref = self.reference_operand(&func);

            let block = self.make_bb_for_call(
                stringify!(pri::enter_func),
                vec![operand::copy_for_local(func_ref.into())],
            );
            self.insert_blocks([block]);
        }

        fn return_from_func(&mut self) {
            let block = self.make_bb_for_call(stringify!(pri::return_from_func), vec![]);
            self.insert_blocks([block]);
        }

        fn after_call_func(&mut self, destination: &Place<'tcx>) {
            // we want the place reference to be after the function call as well
            let BlocksAndResult(mut blocks, dest_ref) = self.internal_reference_place(destination);
            let after_call_block = self.make_bb_for_call(
                stringify!(pri::after_call_func),
                vec![operand::copy_for_local(dest_ref)],
            );
            blocks.push(after_call_block);
            debug_assert_matches!(
                self.context.insertion_loc(),
                After(..),
                "Inserting after_call before a block is not expected."
            );
            self.insert_blocks(blocks);
        }
    }

    impl<'tcx, C> AssertionHandler<'tcx> for RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: ForAssertion<'tcx>,
    {
        fn check_assert(
            &mut self,
            cond: OperandRef,
            expected: bool,
            msg: &rustc_middle::mir::AssertMessage<'tcx>,
        ) {
            let (func_name, mut additional_operands, additional_stmts) =
                self.reference_assert_kind(msg);

            let mut operands = vec![
                operand::move_for_local(cond.into()),
                // this is a compile-time known value, so we can just pass it!
                // NOTE: we could call different functions based on the value of this to improve
                //       performance, but it wouldn't really affect much...
                operand::const_from_bool(self.context.tcx(), expected),
            ];
            operands.append(&mut additional_operands);

            let mut block = self.make_bb_for_call(func_name, operands);
            block.statements.extend(additional_stmts);
            self.insert_blocks([block]);
        }

        fn reference_assert_kind(
            &mut self,
            msg: &rustc_middle::mir::AssertMessage<'tcx>,
        ) -> (&'static str, Vec<Operand<'tcx>>, Vec<Statement<'tcx>>) {
            use rustc_middle::mir::AssertKind;
            match msg {
                AssertKind::BoundsCheck { len, index } => {
                    let len_ref = self.reference_operand(len);
                    let index_ref = self.reference_operand(index);
                    (
                        stringify!(pri::check_assert_bounds_check),
                        vec![
                            operand::copy_for_local(len_ref.into()),
                            operand::copy_for_local(index_ref.into()),
                        ],
                        vec![],
                    )
                }
                AssertKind::Overflow(bin_op, op1, op2) => {
                    let binary_op_ty = self.context.pri_types().binary_op;
                    let operator = convert_mir_binop_to_pri(bin_op);
                    let operator_local = self.context.add_local(binary_op_ty);
                    let additional_stmts = enums::set_variant_to_local(
                        self.tcx(),
                        binary_op_ty,
                        operator as u128,
                        operator_local,
                    );

                    let op1_ref = self.reference_operand(op1);
                    let op2_ref = self.reference_operand(op2);
                    (
                        stringify!(pri::check_assert_overflow),
                        vec![
                            // TODO: double check that moves and copies here are correct
                            operand::move_for_local(operator_local),
                            operand::copy_for_local(op1_ref.into()),
                            operand::copy_for_local(op2_ref.into()),
                        ],
                        additional_stmts,
                    )
                }
                AssertKind::OverflowNeg(op) => {
                    let op_ref = self.reference_operand(op);
                    (
                        stringify!(pri::check_assert_overflow_neg),
                        vec![operand::copy_for_local(op_ref.into())],
                        vec![],
                    )
                }
                AssertKind::DivisionByZero(op) => {
                    let op_ref = self.reference_operand(op);
                    (
                        stringify!(pri::check_assert_div_by_zero),
                        vec![operand::copy_for_local(op_ref.into())],
                        vec![],
                    )
                }
                AssertKind::RemainderByZero(op) => {
                    let op_ref = self.reference_operand(op);
                    (
                        stringify!(pri::check_assert_rem_by_zero),
                        vec![operand::copy_for_local(op_ref.into())],
                        vec![],
                    )
                }
                AssertKind::ResumedAfterReturn(_generator_kind) => {
                    // NOTE: check if these exist in HIR only
                    todo!("research if this is unreachable or not; likely it's not reachable")
                }
                AssertKind::ResumedAfterPanic(_generator_kind) => {
                    todo!("research if this is unreachable or not; likely it's not reachable")
                }
                AssertKind::MisalignedPointerDereference { .. } => {
                    todo!("investigate when pointers are supported")
                }
            }
        }
    }

    impl<'tcx, C> EntryFunctionHandler for RuntimeCallAdder<C>
    where
        Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
        C: ForEntryFunction<'tcx>,
    {
        fn init_runtime_lib(&mut self) {
            let block = self.make_bb_for_call(stringify!(pri::init_runtime_lib), vec![]);
            self.insert_blocks([block]);
        }
    }

    struct BlocksAndResult<'tcx>(Vec<BasicBlockData<'tcx>>, Local);

    impl<'tcx> BlocksAndResult<'tcx> {
        fn prepend(self, blocks: impl IntoIterator<Item = BasicBlockData<'tcx>>) -> Self {
            Self(blocks.into_iter().chain(self.0).collect(), self.1)
        }
    }

    impl<'tcx> From<(BasicBlockData<'tcx>, Local)> for BlocksAndResult<'tcx> {
        fn from(value: (BasicBlockData<'tcx>, Local)) -> Self {
            BlocksAndResult(vec![value.0], value.1)
        }
    }

    mod utils {
        use rustc_middle::{
            mir::{
                self, BasicBlock, BasicBlockData, Constant, HasLocalDecls, Local, Operand, Place,
                Rvalue, SourceInfo, Statement,
            },
            ty::{Ty, TyCtxt, TyKind},
        };
        use rustc_span::DUMMY_SP;

        use crate::mir_transform::{BodyLocalManager, NEXT_BLOCK};

        pub(super) use self::assignment::rvalue;

        use super::{context::PriItemsProvider, DefId};

        pub(super) mod operand {
            use std::mem::size_of;

            use rustc_const_eval::interpret::Scalar;
            use rustc_middle::{
                mir::ConstantKind,
                ty::{self, ScalarInt},
            };
            use rustc_type_ir::UintTy;

            use super::*;

            fn uint_ty_from_bytes(size: usize) -> UintTy {
                [
                    UintTy::U8,
                    UintTy::U16,
                    UintTy::U32,
                    UintTy::U64,
                    UintTy::U128,
                ]
                .into_iter()
                .find(|t| (t.bit_width().unwrap() / 8) as usize == size)
                .unwrap()
            }

            pub fn const_from_existing_ty_const(constant: ty::Const) -> Operand {
                const_from_existing(&Box::new(Constant {
                    span: DUMMY_SP,
                    user_ty: None,
                    literal: ConstantKind::Ty(constant),
                }))
            }

            #[allow(clippy::borrowed_box)]
            pub fn const_from_existing<'tcx>(constant: &Box<Constant<'tcx>>) -> Operand<'tcx> {
                Operand::Constant(constant.clone())
            }

            pub fn const_from_uint<T>(tcx: TyCtxt, value: T) -> Operand
            where
                T: Into<u128>,
            {
                const_from_scalar_int(
                    tcx,
                    ScalarInt::try_from_uint(value, rustc_abi::Size::from_bytes(size_of::<T>()))
                        .unwrap(),
                    tcx.mk_mach_uint(uint_ty_from_bytes(size_of::<T>())),
                )
            }

            pub fn const_from_bool(tcx: TyCtxt, value: bool) -> Operand {
                const_from_scalar_int(tcx, ScalarInt::from(value), tcx.types.bool)
            }

            pub fn const_from_char(tcx: TyCtxt, value: char) -> Operand {
                const_from_scalar_int(tcx, ScalarInt::from(value), tcx.types.char)
            }

            pub fn const_from_scalar_int<'tcx>(
                tcx: TyCtxt<'tcx>,
                value: ScalarInt,
                ty: Ty<'tcx>,
            ) -> Operand<'tcx> {
                Operand::const_from_scalar(tcx, ty, Scalar::Int(value), DUMMY_SP)
            }

            pub fn const_try_as_unevaluated<'tcx>(
                constant: &Constant<'tcx>,
            ) -> Option<mir::UnevaluatedConst<'tcx>> {
                match constant.literal {
                    ConstantKind::Unevaluated(c, _) => Some(c),
                    ConstantKind::Ty(c) => match c.kind() {
                        ty::ConstKind::Unevaluated(ty::UnevaluatedConst { def, substs }) => {
                            Some(mir::UnevaluatedConst {
                                def,
                                substs,
                                promoted: None,
                            })
                        }
                        _ => None,
                    },
                    _ => None,
                }
            }

            pub fn const_try_as_ptr<'tcx>(
                constant: &Constant<'tcx>,
            ) -> Option<mir::interpret::AllocId> {
                if let ConstantKind::Val(value, _) = constant.literal {
                    if let Some(Scalar::Ptr(ptr, _)) = value.try_to_scalar() {
                        return Some(ptr.provenance);
                    }
                }

                None
            }

            pub fn copy_for_local<'tcx>(value: Local) -> Operand<'tcx> {
                for_local(value, true)
            }

            pub fn move_for_local<'tcx>(value: Local) -> Operand<'tcx> {
                for_local(value, false)
            }

            pub fn for_local<'tcx>(value: Local, copy: bool) -> Operand<'tcx> {
                let place = Place::from(value);
                if copy {
                    Operand::Copy(place)
                } else {
                    Operand::Move(place)
                }
            }

            pub fn func(tcx: TyCtxt, def_id: DefId) -> Operand {
                Operand::function_handle(tcx, def_id, std::iter::empty(), DUMMY_SP)
            }
        }

        pub(super) mod enums {
            use rustc_target::abi::VariantIdx;

            use super::*;

            pub fn set_variant_to_local<'tcx>(
                tcx: TyCtxt<'tcx>,
                enum_ty: Ty<'tcx>,
                variant_discr: u128,
                local: Local,
            ) -> Vec<Statement<'tcx>> {
                let place = Place::from(local);

                let deinit = Statement {
                    source_info: SourceInfo::outermost(DUMMY_SP),
                    kind: rustc_middle::mir::StatementKind::Deinit(Box::new(place)),
                };

                let disc = Statement {
                    source_info: SourceInfo::outermost(DUMMY_SP),
                    kind: rustc_middle::mir::StatementKind::SetDiscriminant {
                        place: Box::new(place),
                        variant_index: get_variant_index_by_discr(tcx, enum_ty, variant_discr),
                    },
                };

                vec![deinit, disc]
            }

            pub fn get_variant_index_by_discr<'tcx>(
                tcx: TyCtxt<'tcx>,
                ty: Ty<'tcx>,
                discr: u128,
            ) -> VariantIdx {
                let adt_def = match ty.kind() {
                    TyKind::Adt(def, _) => def,
                    _ => unreachable!(),
                };
                adt_def
                    .discriminants(tcx)
                    .find(|(_, d)| d.val == discr)
                    .expect("Discriminant value is not valid.")
                    .0
            }
        }

        pub(super) mod assignment {
            use rustc_middle::mir::StatementKind;

            use super::*;

            pub mod rvalue {

                use rustc_index::IndexVec;
                use rustc_middle::{
                    mir::{AggregateKind, BorrowKind, CastKind, Operand, Place, Rvalue},
                    ty::{adjustment::PointerCast, Ty, TyCtxt},
                };

                pub fn ref_of<'tcx>(target: Place<'tcx>, tcx: TyCtxt<'tcx>) -> Rvalue<'tcx> {
                    Rvalue::Ref(tcx.lifetimes.re_erased, BorrowKind::Shared, target)
                }

                pub fn cast_to_unsize<'tcx>(
                    operand: Operand<'tcx>,
                    to_ty: Ty<'tcx>,
                ) -> Rvalue<'tcx> {
                    Rvalue::Cast(CastKind::Pointer(PointerCast::Unsize), operand, to_ty)
                }

                pub fn cast_expose_addr<'tcx>(
                    operand: Operand<'tcx>,
                    to_ty: Ty<'tcx>,
                ) -> Rvalue<'tcx> {
                    Rvalue::Cast(CastKind::PointerExposeAddress, operand, to_ty)
                }

                pub fn array<'tcx>(ty: Ty<'tcx>, items: Vec<Operand<'tcx>>) -> Rvalue<'tcx> {
                    Rvalue::Aggregate(
                        Box::new(AggregateKind::Array(ty)),
                        IndexVec::from_raw(items),
                    )
                }
            }

            pub fn create<'tcx>(destination: Place<'tcx>, value: Rvalue<'tcx>) -> Statement<'tcx> {
                Statement {
                    source_info: SourceInfo::outermost(DUMMY_SP),
                    kind: StatementKind::Assign(Box::new((destination, value))),
                }
            }
        }

        pub(super) mod ty {
            use rustc_abi::Size;
            use rustc_middle::ty::{ParamEnv, ParamEnvAnd};

            use super::*;

            pub fn mk_imm_ref<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Ty<'tcx> {
                tcx.mk_imm_ref(tcx.lifetimes.re_erased, ty)
            }

            pub fn size_of<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Size {
                tcx.layout_of(ParamEnvAnd {
                    param_env: ParamEnv::empty(),
                    value: ty,
                })
                .unwrap()
                .size
            }

            pub fn ebit_sbit_size<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> (u64, u64) {
                use rustc_apfloat::{
                    ieee::{Double, Single},
                    Float,
                };
                assert!(ty.is_floating_point());
                let bit_size = size_of(tcx, ty).bits();
                let ebit_size = if bit_size == Single::BITS as u64 {
                    Single::PRECISION
                } else {
                    Double::PRECISION
                } as u64;
                (ebit_size, bit_size - ebit_size)
            }

            #[inline]
            pub fn is_ref_assignable<'tcx>(ty: &Ty<'tcx>, from: &Ty<'tcx>) -> bool {
                match (ty.kind(), from.kind()) {
                    (TyKind::Ref(_, ty, _), TyKind::Ref(_, from, _)) => is_ref_assignable(ty, from),
                    _ => ty == from,
                }
            }
        }

        pub(super) mod terminator {
            use rustc_middle::mir::{Terminator, TerminatorKind, UnwindAction};

            use super::*;

            pub fn call<'tcx>(
                tcx: TyCtxt<'tcx>,
                func_def_id: DefId,
                args: Vec<Operand<'tcx>>,
                destination: Place<'tcx>,
                target: Option<BasicBlock>,
            ) -> Terminator<'tcx> {
                Terminator {
                    source_info: SourceInfo::outermost(DUMMY_SP),
                    kind: TerminatorKind::Call {
                        /* NOTE: Check if it is supposed to be the same operand for each function definition,
                         * i.e. caching/lazy singleton. */
                        func: operand::func(tcx, func_def_id),
                        args,
                        destination,
                        target: Some(target.unwrap_or(NEXT_BLOCK)),
                        unwind: UnwindAction::Continue,
                        call_source: mir::CallSource::Normal,
                        fn_span: DUMMY_SP,
                    },
                }
            }
        }

        pub(super) fn prepare_operand_for_slice<'tcx>(
            tcx: TyCtxt<'tcx>,
            local_manager: &mut (impl BodyLocalManager<'tcx> + HasLocalDecls<'tcx>),
            item_ty: Ty<'tcx>,
            items: Vec<Operand<'tcx>>,
        ) -> (Local, [Statement<'tcx>; 3]) {
            let array_ty = tcx.mk_array(item_ty, items.len() as u64);
            let array_local = local_manager.add_local(array_ty);
            let array_assign =
                assignment::create(Place::from(array_local), rvalue::array(item_ty, items));

            let ref_ty = ty::mk_imm_ref(tcx, array_ty);
            let ref_local = local_manager.add_local(ref_ty);
            let ref_assign = assignment::create(
                Place::from(ref_local),
                rvalue::ref_of(Place::from(array_local), tcx),
            );

            let (cast_local, cast_assign) = cast_array_ref_to_slice(
                tcx,
                local_manager,
                operand::move_for_local(ref_local),
                Some(ref_ty),
            );

            (cast_local, [array_assign, ref_assign, cast_assign])
        }

        pub(super) fn cast_array_ref_to_slice<'tcx>(
            tcx: TyCtxt<'tcx>,
            local_manager: &mut (impl BodyLocalManager<'tcx> + HasLocalDecls<'tcx>),
            ref_operand: Operand<'tcx>,
            operand_ty: Option<Ty<'tcx>>,
        ) -> (Local, Statement<'tcx>) {
            let TyKind::Ref(region, array_ty, mutability) = operand_ty
                .unwrap_or_else(|| ref_operand.ty(local_manager, tcx))
                .kind()
            else {
                panic!("Operand was not from reference type.")
            };
            debug_assert!(array_ty.is_array());
            let item_ty = array_ty.sequence_element_type(tcx);
            let slice_ty =
                tcx.mk_ty_from_kind(TyKind::Ref(*region, tcx.mk_slice(item_ty), *mutability));

            let local: Local = local_manager.add_local(slice_ty);
            let assignment = assignment::create(
                Place::from(local),
                rvalue::cast_to_unsize(ref_operand, slice_ty),
            );
            (local, assignment)
        }

        #[allow(clippy::borrowed_box)]
        pub(super) fn cast_int_to_bit_rep<'tcx>(
            tcx: TyCtxt<'tcx>,
            context: &mut impl BodyLocalManager<'tcx>,
            constant: &Box<Constant<'tcx>>,
        ) -> (Local, [Statement<'tcx>; 1]) {
            debug_assert!(constant.literal.ty().is_integral());
            let bit_rep_local = context.add_local(tcx.types.u128);
            let bit_rep_assign = assignment::create(
                Place::from(bit_rep_local),
                Rvalue::Cast(
                    mir::CastKind::IntToInt,
                    operand::const_from_existing(constant),
                    tcx.types.u128,
                ),
            );
            (bit_rep_local, [bit_rep_assign])
        }

        #[allow(clippy::borrowed_box)]
        pub(super) fn cast_float_to_bit_rep<'tcx>(
            tcx: TyCtxt<'tcx>,
            context: &mut (impl BodyLocalManager<'tcx> + PriItemsProvider<'tcx>),
            constant: &Box<Constant<'tcx>>,
        ) -> (Local, BasicBlockData<'tcx>) {
            use rustc_middle::ty::FloatTy;
            debug_assert!(constant.ty().is_floating_point());
            let bit_rep_local = context.add_local(tcx.types.u128);
            let conversion_func = if matches!(constant.ty().kind(), TyKind::Float(FloatTy::F32)) {
                context.pri_helper_funcs().f32_to_bits
            } else {
                context.pri_helper_funcs().f64_to_bits
            };
            let block = BasicBlockData::new(Some(terminator::call(
                tcx,
                conversion_func,
                vec![operand::const_from_existing(constant)],
                bit_rep_local.into(),
                None,
            )));
            (bit_rep_local, block)
        }

        pub(super) fn convert_mir_binop_to_pri(op: &mir::BinOp) -> runtime::abs::BinaryOp {
            // FIXME: #197: Add support for unchecked operations.
            match op {
                mir::BinOp::Add => runtime::abs::BinaryOp::Add,
                mir::BinOp::AddUnchecked => runtime::abs::BinaryOp::Add,
                mir::BinOp::Sub => runtime::abs::BinaryOp::Sub,
                mir::BinOp::SubUnchecked => runtime::abs::BinaryOp::Sub,
                mir::BinOp::Mul => runtime::abs::BinaryOp::Mul,
                mir::BinOp::MulUnchecked => runtime::abs::BinaryOp::Mul,
                mir::BinOp::Div => runtime::abs::BinaryOp::Div,
                mir::BinOp::Rem => runtime::abs::BinaryOp::Rem,
                mir::BinOp::BitXor => runtime::abs::BinaryOp::BitXor,
                mir::BinOp::BitAnd => runtime::abs::BinaryOp::BitAnd,
                mir::BinOp::BitOr => runtime::abs::BinaryOp::BitOr,
                mir::BinOp::Shl => runtime::abs::BinaryOp::Shl,
                mir::BinOp::ShlUnchecked => runtime::abs::BinaryOp::Shl,
                mir::BinOp::Shr => runtime::abs::BinaryOp::Shr,
                mir::BinOp::ShrUnchecked => runtime::abs::BinaryOp::Shr,
                mir::BinOp::Eq => runtime::abs::BinaryOp::Eq,
                mir::BinOp::Lt => runtime::abs::BinaryOp::Lt,
                mir::BinOp::Le => runtime::abs::BinaryOp::Le,
                mir::BinOp::Ne => runtime::abs::BinaryOp::Ne,
                mir::BinOp::Ge => runtime::abs::BinaryOp::Ge,
                mir::BinOp::Gt => runtime::abs::BinaryOp::Gt,
                mir::BinOp::Offset => runtime::abs::BinaryOp::Offset,
            }
        }

        pub(super) fn convert_mir_unop_to_pri(op: &mir::UnOp) -> runtime::abs::UnaryOp {
            match op {
                mir::UnOp::Not => runtime::abs::UnaryOp::Not,
                mir::UnOp::Neg => runtime::abs::UnaryOp::Neg,
            }
        }
    }

    /*
     * Context requirements work as aliases for context traits to guarantee that a
     * certain feature will be available in `RuntimeCallAdder` when its context
     * implement that set of traits.
     */
    pub(crate) mod context_requirements {
        use super::{context::*, *};

        pub(crate) trait Basic<'tcx>: BaseContext<'tcx> {}
        impl<'tcx, C> Basic<'tcx> for C where C: BaseContext<'tcx> {}

        pub(crate) trait ForInsertion<'tcx>:
            Basic<'tcx> + BodyProvider<'tcx> + InsertionLocationProvider
        {
        }
        impl<'tcx, C> ForInsertion<'tcx> for C where
            C: Basic<'tcx> + BodyProvider<'tcx> + InsertionLocationProvider
        {
        }

        pub(crate) trait ForPlaceRef<'tcx>: ForInsertion<'tcx> {}
        impl<'tcx, C> ForPlaceRef<'tcx> for C where C: ForInsertion<'tcx> {}

        pub(crate) trait ForOperandRef<'tcx>:
            ForPlaceRef<'tcx> + ForFunctionCalling<'tcx>
        {
        }
        impl<'tcx, C> ForOperandRef<'tcx> for C where C: ForPlaceRef<'tcx> + ForFunctionCalling<'tcx> {}

        pub(crate) trait ForAssignment<'tcx>:
            ForInsertion<'tcx> + DestinationReferenceProvider
        {
        }
        impl<'tcx, C> ForAssignment<'tcx> for C where C: ForInsertion<'tcx> + DestinationReferenceProvider {}

        pub(crate) trait ForBranching<'tcx>:
            ForInsertion<'tcx> + JumpTargetModifier
        {
        }
        impl<'tcx, C> ForBranching<'tcx> for C where C: ForInsertion<'tcx> + JumpTargetModifier {}

        pub(crate) trait ForAssertion<'tcx>: ForOperandRef<'tcx> {}
        impl<'tcx, C> ForAssertion<'tcx> for C where C: ForOperandRef<'tcx> {}

        pub(crate) trait ForFunctionCalling<'tcx>:
            ForPlaceRef<'tcx> + JumpTargetModifier
        {
        }
        impl<'tcx, C> ForFunctionCalling<'tcx> for C where C: ForPlaceRef<'tcx> + JumpTargetModifier {}

        pub(crate) trait ForReturning<'tcx>: ForInsertion<'tcx> {}
        impl<'tcx, C> ForReturning<'tcx> for C where C: ForInsertion<'tcx> {}

        pub(crate) trait ForEntryFunction<'tcx>:
            ForInsertion<'tcx> + InEntryFunction
        {
        }
        impl<'tcx, C> ForEntryFunction<'tcx> for C where C: ForInsertion<'tcx> + InEntryFunction {}
    }
}

pub(super) use implementation::context_requirements as ctxtreqs;
