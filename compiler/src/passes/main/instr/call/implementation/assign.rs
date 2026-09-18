use std::debug_assert_matches;

use rustc_abi::{FieldIdx, VariantIdx};
use rustc_middle::{
    mir::{AggregateKind, BinOp, BorrowKind, CastKind, RawPtrKind, Rvalue, UnOp, WithRetag},
    ty::{self as mir_ty, Const},
};
use rustc_span::def_id::DefId;

use common::{log_debug, log_warn};

use crate::visit::RvalueVisitor;

use super::{
    super::{AssignmentHandler, pri::FunctionInfo},
    OperandReferencer, PlaceReferencer,
    context::{ConfigProvider, SourceInfoProvider},
    ctxt_reqs::{ForAssignment, ForOperandRef},
    prelude::{mir::*, *},
};

use super::super::super::TAG_INSTR;
use super::super::super::decision::rules::EventDecision;

impl<'tcx, C> AssignmentHandler<'tcx> for RuntimeCallAdder<C>
where
    C: ForAssignment<'tcx> + ForOperandRef<'tcx>,
{
    fn to_rvalue(&mut self, rvalue: &Rvalue<'tcx>) {
        log_debug!(target: TAG_INSTR, "Visiting Rvalue: {:#?}", rvalue);

        let filter = self.assignment_filter(rvalue);

        match filter {
            EventDecision::Omit => return,
            EventDecision::Opaque | EventDecision::Detailed => match filter {
                EventDecision::Detailed => self.visit_rvalue(rvalue),
                EventDecision::Opaque => self.add_opaque_assignment(),
                _ => unreachable!(),
            },
        }
    }

    fn its_discriminant_to(&mut self, variant_index: &VariantIdx) {
        let tcx = self.tcx();
        self.add_bb_for_assign_call(
            sym::set_discriminant,
            vec![operand::const_from_uint(tcx, variant_index.as_u32())],
        );
    }
}

impl<'tcx, C> RvalueVisitor<'tcx, ()> for RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForAssignment<'tcx> + ForOperandRef<'tcx>,
{
    fn visit_use(&mut self, operand: &Operand<'tcx>, _: &WithRetag) {
        let operand = self.reference_operand(operand);
        self.add_assignment_use_call(operand)
    }

    fn visit_repeat(&mut self, operand: &Operand<'tcx>, count: &Const<'tcx>) {
        let operand = self.reference_operand(operand);
        self.add_bb_for_assign_call(
            sym::assign_repeat,
            vec![
                operand::copy_for_local(operand.into()),
                #[allow(clippy::clone_on_copy)]
                operand::const_from_existing_ty_const(self.tcx().types.usize, count.clone()),
            ],
        )
    }

    fn visit_ref(
        &mut self,
        _region: &mir_ty::Region,
        borrow_kind: &BorrowKind,
        place: &Place<'tcx>,
    ) {
        let place = self.reference_place(place);
        self.add_bb_for_assign_call(
            sym::assign_ref,
            vec![
                operand::copy_for_local(place.into()),
                operand::const_from_bool(
                    self.context.tcx(),
                    matches!(borrow_kind, BorrowKind::Mut { .. }),
                ),
            ],
        )
    }

    fn visit_thread_local_ref(&mut self, _def_id: &DefId) {
        self.add_bb_for_assign_call(sym::assign_thread_local_ref, vec![])
    }

    fn visit_raw_ptr(&mut self, kind: &RawPtrKind, place: &Place<'tcx>) {
        let place = self.reference_place(place);
        self.add_bb_for_assign_call(
            sym::assign_raw_ptr_of,
            vec![
                operand::copy_for_local(place.into()),
                operand::const_from_bool(self.context.tcx(), kind.to_mutbl_lossy().is_mut()),
            ],
        )
    }

    fn visit_cast(&mut self, kind: &CastKind, operand: &Operand<'tcx>, ty: &Ty<'tcx>) {
        let operand = self.reference_operand(operand);
        use CastKind::*;
        match kind {
            IntToInt | FloatToInt => self.by_cast_to_int(operand, *ty),
            IntToFloat | FloatToFloat => self.by_cast_to_float(operand, *ty),
            PointerCoercion(coercion, _source) => {
                use mir_ty::adjustment::PointerCoercion::*;
                match coercion {
                    Unsize => self.by_cast_through_unsizing(operand),
                    ReifyFnPointer(_) | UnsafeFnPointer | ClosureFnPointer(_) => {
                        self.by_cast_through_fn_ptr_coercion(operand)
                    }
                    MutToConstPointer => self.by_cast_to_another_ptr(operand, *ty, *kind),
                    ArrayToPointer => {
                        log_warn!(
                            target: TAG_INSTR,
                            concat!(
                                "ArrayToPointer casts are expected to be optimized away by at this point.",
                                "Sending it to runtime as a regular pointer cast."
                            )
                        );
                        self.by_cast_to_another_ptr(operand, *ty, *kind)
                    }
                }
            }
            PointerExposeProvenance => self.by_cast_expose_prov(operand),
            PointerWithExposedProvenance => self.by_cast_with_exposed_prov(operand, *ty),
            PtrToPtr | FnPtrToPtr => self.by_cast_to_another_ptr(operand, *ty, *kind),
            Transmute => self.by_cast_transmuted(operand, *ty),
            Subtype => self.by_cast_subtyped(operand, *ty),
        }
    }

    fn visit_binary_op(
        &mut self,
        operator: &BinOp,
        operands: &Box<(Operand<'tcx>, Operand<'tcx>)>,
    ) {
        let first = self.reference_operand(&operands.0);
        let second = self.reference_operand(&operands.1);
        self.add_operator_assignment(
            self.context.pri_helper_funcs().const_binary_op_of,
            convert_mir_binop_to_pri(operator).to_raw().into(),
            sym::assign_binary_op,
            vec![
                operand::copy_for_local(first.into()),
                operand::copy_for_local(second.into()),
            ],
        )
    }

    fn visit_unary_op(&mut self, operator: &UnOp, operand: &Operand<'tcx>) {
        let operand = self.reference_operand(operand);
        self.add_operator_assignment(
            self.context.pri_helper_funcs().const_unary_op_of,
            convert_mir_unop_to_pri(operator).to_raw().into(),
            sym::assign_unary_op,
            vec![operand::copy_for_local(operand.into())],
        )
    }

    fn visit_discriminant(&mut self, place: &Place<'tcx>) {
        let place = self.reference_place(place);
        self.add_bb_for_assign_call(
            sym::assign_discriminant,
            vec![operand::copy_for_local(place.into())],
        )
    }

    fn visit_aggregate(
        &mut self,
        kind: &Box<AggregateKind>,
        operands: &rustc_index::IndexVec<FieldIdx, Operand<'tcx>>,
    ) {
        let operands: Vec<OperandRef> = operands
            .iter()
            .map(|operand| self.reference_operand(operand))
            .collect();

        self.add_aggregate_assignment(kind.as_ref(), &operands)
    }

    fn visit_copy_for_deref(&mut self, place: &Place<'tcx>) {
        let operand = Operand::Copy(*place);
        self.visit_use(&operand, &WithRetag::No)
    }

    fn visit_wrap_unsafe_binder(&mut self, operand: &Operand<'tcx>, ty: &Ty<'tcx>) {
        let operand = self.reference_operand(operand);
        let id_local = {
            let (block, id_local) = self.make_type_id_of_bb(*ty);
            self.insert_blocks([block]);
            id_local
        };
        self.add_bb_for_assign_call(
            sym::assign_wrap_unsafe_binder,
            vec![
                operand::copy_for_local(operand.into()),
                operand::move_for_local(id_local),
            ],
        )
    }

    fn visit_reborrow(
        &mut self,
        target_ty: &Ty<'tcx>,
        mutability: &rustc_hir::Mutability,
        place: &Place<'tcx>,
    ) {
        panic!(
            concat!(
                "Reborrow is not expected to be observed at this point. ",
                "It should have been optimized away by the compiler. ",
                "({:?}, {:?}, {:?}) at {:?}"
            ),
            target_ty,
            mutability,
            place,
            self.source_info().span,
        );
    }
}

impl<'tcx, C> RuntimeCallAdder<C>
where
    Self: ConfigProvider,
{
    fn assignment_filter(&self, rvalue: &Rvalue<'tcx>) -> EventDecision {
        use EventDecision::*;

        let rules = &self.config().assignment_filter;
        match rvalue {
            Rvalue::Use(..) => rules.use_,
            Rvalue::Repeat(..) => rules.repeat,
            Rvalue::Ref(..) => rules.ref_,
            Rvalue::ThreadLocalRef(..) => rules.thread_local_ref,
            Rvalue::RawPtr(..) => rules.raw_ptr,
            Rvalue::Cast(..) => rules.cast,
            Rvalue::BinaryOp(..) => rules.binary_op,
            Rvalue::UnaryOp(..) => rules.unary_op,
            Rvalue::Discriminant(..) => rules.discriminant,
            Rvalue::Aggregate(..) => rules.aggregate,
            Rvalue::CopyForDeref(..) => rules.use_,
            Rvalue::WrapUnsafeBinder(..) => rules.wrap_unsafe_binder,
            Rvalue::Reborrow(..) => Omit,
        }
    }
}

impl<'tcx, C> RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForAssignment<'tcx>,
{
    fn add_aggregate_assignment(&mut self, kind: &AggregateKind, operands: &[OperandRef]) {
        let add_agg_basic = |this: &mut Self, symbol: LeafSymbol| {
            this.add_bb_for_aggregate_assign_call(symbol, operands, Default::default())
        };
        let add_adt = |this: &mut Self, symbol: LeafSymbol, additional_args: Vec<Operand<'tcx>>| {
            this.add_bb_for_adt_assign_call(symbol, operands, additional_args)
        };
        use AggregateKind::*;
        match kind {
            Array(..) => add_agg_basic(self, sym::assign_aggregate_array),
            Closure(..) => add_agg_basic(self, sym::assign_aggregate_closure),
            Coroutine(..) => add_agg_basic(self, sym::assign_aggregate_coroutine),
            CoroutineClosure(..) => add_agg_basic(self, sym::assign_aggregate_coroutine_closure),
            Tuple | Adt(_, _, _, _, None) => match kind {
                Tuple => add_adt(self, sym::assign_aggregate_tuple, Default::default()),
                Adt(def_id, variant, _, _, None) => {
                    use rustc_hir::def::DefKind;
                    match self.tcx().def_kind(*def_id) {
                        DefKind::Enum => {
                            let variant =
                                operand::const_from_uint(self.context.tcx(), variant.as_u32());
                            add_adt(self, sym::assign_aggregate_enum, vec![variant])
                        }
                        DefKind::Struct => {
                            add_adt(self, sym::assign_aggregate_struct, Default::default())
                        }
                        kind => unreachable!("Unexpected ADT kind: {:?}", kind),
                    }
                }
                _ => unreachable!(),
            },
            // Union
            Adt(_, _, _, _, Some(active_field)) => {
                assert_eq!(operands.len(), 1);
                self.add_bb_for_assign_call_with_statements(
                    sym::assign_aggregate_union,
                    vec![
                        operand::const_from_uint(self.context.tcx(), active_field.as_u32()),
                        operand::copy_for_local(operands[0].into()),
                    ],
                    vec![],
                )
            }
            RawPtr(_, mutability) => match operands {
                [data_ptr, metadata] => self.add_bb_for_assign_call_with_statements(
                    sym::assign_aggregate_raw_ptr,
                    vec![
                        operand::move_for_local((*data_ptr).into()),
                        operand::move_for_local((*metadata).into()),
                        operand::const_from_bool(self.tcx(), mutability.is_mut()),
                    ],
                    vec![],
                ),
                _ => unreachable!(),
            },
        }
    }

    fn add_operator_assignment(
        &mut self,
        operator_func: FunctionInfo,
        operator: u128,
        assignment_func: LeafSymbol,
        mut args: Vec<Operand<'tcx>>,
    ) {
        let (block, operator_local) = self.make_bb_for_helper_call_with_all(
            operator_func,
            vec![],
            vec![operand::const_from_uint(self.tcx(), operator)],
            Default::default(),
        );
        self.insert_blocks([block]);

        args.insert(0, operand::move_for_local(operator_local));
        self.add_bb_for_assign_call(assignment_func, args)
    }

    pub(super) fn add_assignment_use_call(&mut self, operand: OperandRef) {
        self.add_bb_for_assign_call(
            sym::assign_use,
            vec![operand::copy_for_local(operand.into())],
        )
    }

    pub(crate) fn add_opaque_assignment(&mut self) {
        self.add_bb_for_assign_call(sym::assign_some, vec![])
    }
}

impl<'tcx, C> RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForAssignment<'tcx>,
{
    fn add_bb_for_aggregate_assign_call(
        &mut self,
        func_name: LeafSymbol,
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

    fn add_bb_for_adt_assign_call(
        &mut self,
        func_name: LeafSymbol,
        fields: &[OperandRef],
        additional_args: Vec<Operand<'tcx>>,
    ) {
        let mut args = Vec::new();
        let mut additional_stmts = Vec::new();

        let (fields_local, fields_stmts) = self.make_slice_for_adt_elements(fields);
        args.push(operand::move_for_local(fields_local));
        additional_stmts.extend(fields_stmts);

        args.extend(additional_args);

        self.add_bb_for_assign_call_with_statements(func_name, args, additional_stmts)
    }

    fn make_slice_for_adt_elements(
        &mut self,
        elements: &[OperandRef],
    ) -> (Local, [Statement<'tcx>; 3]) {
        let operand_ref_ty = self.context.pri_types().operand_ref(self.tcx());
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

    fn add_bb_for_assign_call(&mut self, func_name: LeafSymbol, args: Vec<Operand<'tcx>>) {
        self.add_bb_for_assign_call_with_statements(func_name, args, vec![])
    }

    fn add_bb_for_assign_call_with_statements(
        &mut self,
        func_name: LeafSymbol,
        args: Vec<Operand<'tcx>>,
        statements: Vec<Statement<'tcx>>,
    ) {
        let dest_ref = self.reference_destination();
        let mut block = self.make_bb_for_assign_call(func_name, dest_ref, args);
        block.statements.extend(statements);
        self.insert_blocks([block]);
    }

    pub(super) fn make_bb_for_assign_call(
        &mut self,
        func_name: LeafSymbol,
        dest_ref: PlaceRef,
        args: Vec<Operand<'tcx>>,
    ) -> BasicBlockData<'tcx> {
        let assignment_id = self.context.assignment_id();
        self.make_bb_for_call(
            func_name,
            [
                vec![
                    operand::const_from_uint(self.tcx(), assignment_id),
                    operand::copy_for_local(dest_ref.into()),
                ],
                args,
            ]
            .concat(),
        )
    }
}

impl<'tcx, C> RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForAssignment<'tcx>,
{
    fn by_cast_to_int(&mut self, operand: OperandRef, ty: Ty<'tcx>) {
        if ty.is_char() {
            self.add_bb_for_cast_assign_call(operand, sym::assign_cast_char)
        } else {
            assert!(ty.is_integral());

            let tcx = self.context.tcx();
            let is_signed = ty.is_signed();
            let bits = ty.primitive_size(tcx).bits();

            self.add_bb_for_cast_assign_call_with_args(
                operand,
                sym::assign_cast_integer,
                vec![
                    operand::const_from_uint(tcx, bits),
                    operand::const_from_bool(tcx, is_signed),
                ],
            )
        }
    }

    fn by_cast_to_float(&mut self, operand: OperandRef, ty: Ty<'tcx>) {
        let (e_bits, s_bits) = ty::ebit_sbit_size(ty);
        self.add_bb_for_cast_assign_call_with_args(
            operand,
            sym::assign_cast_float,
            vec![
                operand::const_from_uint(self.context.tcx(), e_bits),
                operand::const_from_uint(self.context.tcx(), s_bits),
            ],
        )
    }

    fn by_cast_through_unsizing(&mut self, operand: OperandRef) {
        self.add_bb_for_cast_assign_call(operand, sym::assign_cast_unsize)
    }

    fn by_cast_through_fn_ptr_coercion(&mut self, operand: OperandRef) {
        // Effective only at compile time, no operational effect.
        self.add_assignment_use_call(operand)
    }

    fn by_cast_expose_prov(&mut self, operand: OperandRef) {
        self.add_bb_for_cast_assign_call(operand, sym::assign_cast_expose_prov);
    }

    fn by_cast_with_exposed_prov(&mut self, operand: OperandRef, ty: Ty<'tcx>) {
        self.add_bb_for_pointer_cast_assign_call(operand, ty, sym::assign_cast_with_exposed_prov);
    }

    fn by_cast_to_another_ptr(&mut self, operand: OperandRef, ty: Ty<'tcx>, kind: CastKind) {
        use CastKind::*;
        use rustc_middle::ty::adjustment::PointerCoercion::*;
        debug_assert_matches!(
            kind,
            PtrToPtr | FnPtrToPtr | PointerCoercion(MutToConstPointer | ArrayToPointer, _)
        );
        /* NOTE: Currently, we do not distinguish between different pointer casts.
         * This is because they all keep the data untouched and are just about
         * semantics. We can add support for them later if interested. */
        self.add_bb_for_pointer_cast_assign_call(operand, ty, sym::assign_cast_to_another_ptr);
    }

    fn by_cast_transmuted(&mut self, operand: OperandRef, ty: Ty<'tcx>) {
        let id_local = {
            let (block, id_local) = self.make_type_id_of_bb(ty);
            self.insert_blocks([block]);
            id_local
        };
        self.add_bb_for_cast_assign_call_with_args(
            operand,
            sym::assign_cast_transmute,
            vec![operand::move_for_local(id_local)],
        )
    }

    fn by_cast_subtyped(&mut self, operand: OperandRef, ty: Ty<'tcx>) {
        let id_local = {
            let (block, id_local) = self.make_type_id_of_bb(ty);
            self.insert_blocks([block]);
            id_local
        };
        self.add_bb_for_cast_assign_call_with_args(
            operand,
            sym::assign_cast_subtype,
            vec![operand::move_for_local(id_local)],
        )
    }
}

impl<'tcx, C> RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForAssignment<'tcx>,
{
    fn add_bb_for_cast_assign_call(&mut self, operand: OperandRef, func_name: LeafSymbol) {
        self.add_bb_for_cast_assign_call_with_args(operand, func_name, vec![])
    }

    fn add_bb_for_cast_assign_call_with_args(
        &mut self,
        operand: OperandRef,
        func_name: LeafSymbol,
        args: Vec<Operand<'tcx>>,
    ) {
        self.add_bb_for_assign_call(
            func_name,
            [vec![operand::copy_for_local(operand.into())], args].concat(),
        )
    }

    fn add_bb_for_pointer_cast_assign_call(
        &mut self,
        operand: OperandRef,
        ty: Ty<'tcx>,
        func_name: LeafSymbol,
    ) {
        let id_local: Local = {
            let (block, id_local) = self.make_type_id_of_bb(ty);
            self.insert_blocks([block]);
            id_local
        };
        self.add_bb_for_cast_assign_call_with_args(
            operand,
            func_name,
            vec![operand::move_for_local(id_local)],
        );
    }
}

mod utils {
    pub(super) use super::super::utils::{
        convert_mir_binop_to_pri, convert_mir_unop_to_pri, prepare_operand_for_slice, ty,
    };

    pub(super) mod operand {
        use rustc_middle::{mir::Const, mir::ConstOperand, ty};
        use rustc_span::DUMMY_SP;

        pub use super::super::super::utils::operand::*;

        use super::super::super::prelude::mir::*;

        pub fn const_from_existing_ty_const<'tcx>(
            ty: Ty<'tcx>,
            constant: ty::Const<'tcx>,
        ) -> Operand<'tcx> {
            const_from_existing(&Box::new(ConstOperand {
                span: DUMMY_SP,
                user_ty: None,
                const_: Const::Ty(ty, constant),
            }))
        }
    }
}
use utils::*;
