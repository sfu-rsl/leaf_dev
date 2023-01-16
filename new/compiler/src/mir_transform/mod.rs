use std::{collections::HashMap, fmt::Debug};

use rustc_apfloat::{
    ieee::{self},
    Float,
};
use rustc_const_eval::interpret::{ConstValue, Scalar};
use rustc_hir::def::DefKind;
use rustc_middle::{
    middle::exported_symbols::ExportedSymbol,
    mir::{
        BasicBlock, BasicBlockData, BinOp, Constant, ConstantKind, Local, Operand, Place,
        ProjectionElem, SourceInfo, Statement, Terminator, TerminatorKind, UnOp,
    },
    ty::{ScalarInt, Ty, TyCtxt},
};
use rustc_span::{def_id::DefId, Span, DUMMY_SP};

use self::{modification::BodyModificationUnit, utils::*};

pub mod modification;

struct FunctionInfo<'tcx> {
    def_id: DefId,
    ret_ty: Ty<'tcx>,
}

struct SpecialTypes<'tcx> {
    place_ref: Ty<'tcx>,
    operand_ref: Ty<'tcx>,
    binary_op: Ty<'tcx>,
    unary_op: Ty<'tcx>,
}

pub struct RuntimeCallAdder<'tcx, 'm> {
    tcx: TyCtxt<'tcx>,
    modification_unit: &'m mut BodyModificationUnit<'tcx>,
    pri_functions: HashMap<String, FunctionInfo<'tcx>>,
    pri_special_types: SpecialTypes<'tcx>,
}

impl<'tcx, 'm> RuntimeCallAdder<'tcx, 'm> {
    pub fn new(tcx: TyCtxt<'tcx>, modification_unit: &'m mut BodyModificationUnit<'tcx>) -> Self {
        Self {
            tcx: tcx,
            modification_unit,
            pri_functions: Self::extract_functions(tcx), // FIXME: Perform caching
            pri_special_types: Self::find_special_types(tcx),
        }
    }

    fn extract_functions(tcx: TyCtxt<'tcx>) -> HashMap<String, FunctionInfo<'tcx>> {
        RuntimeCallAdder::get_exported_symbols_of_pri(tcx)
            .into_iter()
            .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Fn))
            .map(|def_id| {
                (
                    tcx.def_path_str(def_id),
                    FunctionInfo {
                        def_id,
                        ret_ty: tcx.fn_sig(def_id).output().skip_binder(),
                    },
                )
            })
            .collect()
    }

    fn find_special_types(tcx: TyCtxt<'tcx>) -> SpecialTypes<'tcx> {
        /*
         * FIXME: The desired enums and type aliases don't show up in the exported symbols.
         * It may be because of the MIR phases that clean up/optimize/unify things,
         * the way that the library is added (using the compiled file), or
         * that enums and type aliases are not included at all in the exported_symbols.
         * As a workaround, we have defined some static variables having those desired
         * types and are accessible.
         * However, there should be some functions in TyCtxt that will list these items for us.
         */
        let def_ids: HashMap<String, DefId> = RuntimeCallAdder::get_exported_symbols_of_pri(tcx)
            .into_iter()
            .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Static(_)))
            .map(|def_id| (tcx.def_path_str(def_id), def_id))
            .collect();

        let get_ty =
            |name: &str| -> Ty<'tcx> { tcx.type_of(def_ids.get(&name.replace(" ", "")).unwrap()) };
        SpecialTypes {
            place_ref: get_ty(stringify!(pri::PLACE_REF_TYPE_HOLDER)),
            operand_ref: get_ty(stringify!(pri::OPERAND_REF_TYPE_HOLDER)),
            binary_op: get_ty(stringify!(pri::BINARY_OP_TYPE_HOLDER)),
            unary_op: get_ty(stringify!(pri::UNARY_OP_TYPE_HOLDER)),
        }
    }

    fn get_exported_symbols_of_pri(tcx: TyCtxt<'tcx>) -> Vec<DefId> {
        let crate_num = tcx
            .crates(())
            .iter()
            .find(|cnum| tcx.crate_name(**cnum).as_str() == stringify!(pri))
            .expect(format!("{} crate is not added as a dependency.", stringify!(pri)).as_str())
            .clone();

        tcx.exported_symbols(crate_num)
            .iter()
            .filter_map(|(exported_symbol, _)| match exported_symbol {
                ExportedSymbol::NonGeneric(def_id) | ExportedSymbol::Generic(def_id, _) => {
                    Some(def_id)
                }
                _ => None,
            })
            .cloned()
            .collect()
    }
}

struct BlocksAndResult<'tcx>(Vec<BasicBlockData<'tcx>>, Local);

impl<'tcx> BlocksAndResult<'tcx> {
    fn prepend(self, blocks: Vec<BasicBlockData<'tcx>>) -> Self {
        Self([blocks, self.0].concat(), self.1)
    }
}

impl<'tcx> From<(BasicBlockData<'tcx>, Local)> for BlocksAndResult<'tcx> {
    fn from(value: (BasicBlockData<'tcx>, Local)) -> Self {
        BlocksAndResult(vec![value.0], value.1)
    }
}

impl<'tcx, 'm> RuntimeCallAdder<'tcx, 'm> {
    pub fn reference_place(&mut self, location: BasicBlock, place: &Place<'tcx>) -> Local {
        let BlocksAndResult(new_blocks, reference) = self.internal_reference_place(place);
        self.modification_unit
            .insert_blocks_before(location, new_blocks);
        reference
    }

    fn internal_reference_place(&mut self, place: &Place<'tcx>) -> BlocksAndResult<'tcx> {
        let mut new_blocks = vec![];
        let (call_block, mut current_ref) = self.make_bb_for_place_ref_call(
            stringify!(pri::ref_place_local),
            vec![operand::const_from_uint(self.tcx, u32::from(place.local))],
        );
        new_blocks.push(call_block);

        for (_, proj) in place.iter_projections() {
            let BlocksAndResult(added_blocks, wrapped_ref) =
                self.reference_place_projection(current_ref, proj);
            current_ref = wrapped_ref;
            new_blocks.extend(added_blocks);
        }

        BlocksAndResult(new_blocks, current_ref)
    }

    fn reference_place_projection<T>(
        &mut self,
        current_ref: Local,
        proj: ProjectionElem<Local, T>,
    ) -> BlocksAndResult<'tcx> {
        let mut new_blocks = Vec::new();

        let (func_name, additional_args) = match proj {
            ProjectionElem::Deref => (stringify!(pri::ref_place_deref), vec![]),
            ProjectionElem::Field(index, _) => (
                stringify!(pri::ref_place_field),
                vec![operand::const_from_uint(self.tcx, u32::from(index))],
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
                    operand::const_from_uint(self.tcx, offset),
                    operand::const_from_uint(self.tcx, min_length),
                    operand::const_from_bool(self.tcx, from_end),
                ],
            ),
            ProjectionElem::Subslice { from, to, from_end } => (
                stringify!(pri::ref_place_subslice),
                vec![
                    operand::const_from_uint(self.tcx, from),
                    operand::const_from_uint(self.tcx, to),
                    operand::const_from_bool(self.tcx, from_end),
                ],
            ),
            ProjectionElem::Downcast(_, index) => (
                stringify!(pri::ref_place_downcast),
                vec![operand::const_from_uint(self.tcx, u32::from(index))],
            ),
            ProjectionElem::OpaqueCast(_) => (stringify!(pri::ref_place_opaque_cast), vec![]),
        };

        BlocksAndResult::from(self.make_bb_for_place_ref_call(
            func_name,
            [vec![operand::copy_for_local(current_ref)], additional_args].concat(),
        ))
        .prepend(new_blocks)
    }

    fn make_bb_for_place_ref_call(
        &mut self,
        func_name: &str,
        args: Vec<Operand<'tcx>>,
    ) -> (BasicBlockData<'tcx>, Local) {
        self.make_bb_for_call_with_ret(func_name, args)
    }
}

impl<'tcx, 'm> RuntimeCallAdder<'tcx, 'm> {
    pub fn reference_operand(&mut self, location: BasicBlock, operand: &Operand<'tcx>) -> Local {
        let BlocksAndResult(new_blocks, reference) = self.internal_reference_operand(operand);
        self.modification_unit
            .insert_blocks_before(location, new_blocks);
        reference
    }

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
            Operand::Constant(constant) => self.internal_reference_const_operand(&constant),
        }
    }

    fn internal_reference_const_operand(
        &mut self,
        constant: &Constant<'tcx>,
    ) -> BlocksAndResult<'tcx> {
        let kind = constant.literal;
        match kind {
            ConstantKind::Ty(_) => todo!(),
            ConstantKind::Unevaluated(_, _) => todo!(),
            ConstantKind::Val(value, ty) => self.internal_reference_val_const_operand(value, ty),
        }
    }

    fn internal_reference_val_const_operand(
        &mut self,
        value: ConstValue<'tcx>,
        ty: Ty<'tcx>,
    ) -> BlocksAndResult<'tcx> {
        match value {
            ConstValue::Scalar(scalar) => self.internal_reference_scalar_const_operand(scalar, ty),
            ConstValue::ZeroSized => todo!(),
            ConstValue::Slice { data, start, end } => todo!(),
            ConstValue::ByRef { alloc, offset } => todo!(),
        }
    }

    fn internal_reference_scalar_const_operand(
        &mut self,
        scalar: Scalar,
        ty: Ty<'tcx>,
    ) -> BlocksAndResult<'tcx> {
        match scalar {
            Scalar::Int(int) => self.internal_reference_scalar_int_const_operand(int, ty),
            Scalar::Ptr(_, _) => todo!(),
        }
    }

    fn internal_reference_scalar_int_const_operand(
        &mut self,
        scalar: ScalarInt,
        ty: Ty<'tcx>,
    ) -> BlocksAndResult<'tcx> {
        if ty.is_bool() {
            self.make_bb_for_operand_ref_call(
                stringify!(pri::ref_operand_const_bool),
                vec![operand::const_from_scalar_int(self.tcx, scalar.clone(), ty)],
            )
        } else if ty.is_integral() {
            self.make_bb_for_operand_ref_call(
                stringify!(pri::ref_operand_const_int),
                vec![
                    operand::const_from_uint(
                        self.tcx,
                        /* Currently no direct way to read the data field. */
                        scalar.assert_bits(scalar.size()),
                    ),
                    operand::const_from_uint(self.tcx, scalar.size().bits()),
                    operand::const_from_bool(self.tcx, ty.is_signed()),
                ],
            )
        } else if ty.is_floating_point() {
            let bit_size = scalar.size().bits();
            let ebit_size = if bit_size == ieee::Single::BITS as u64 {
                ieee::Single::PRECISION
            } else {
                ieee::Double::PRECISION
            } as u64;
            let sbits = bit_size - ebit_size;
            self.make_bb_for_operand_ref_call(
                stringify!(pri::ref_operand_const_float),
                vec![
                    operand::const_from_uint(
                        self.tcx,
                        /* Currently no direct way to read the data field. */
                        scalar.assert_bits(scalar.size()),
                    ),
                    operand::const_from_uint(self.tcx, ebit_size),
                    operand::const_from_uint(self.tcx, sbits),
                ],
            )
        } else if ty.is_char() {
            self.make_bb_for_operand_ref_call(
                stringify!(pri::ref_operand_const_char),
                vec![operand::const_from_scalar_int(self.tcx, scalar.clone(), ty)],
            )
        } else {
            unreachable!("ScalarInt is supposed to be either bool, int, float, or char.")
        }
        .into()
    }

    fn make_bb_for_operand_ref_call(
        &mut self,
        func_name: &str,
        args: Vec<Operand<'tcx>>,
    ) -> (BasicBlockData<'tcx>, Local) {
        self.make_bb_for_call_with_ret(func_name, args)
    }
}

impl<'tcx, 'm> RuntimeCallAdder<'tcx, 'm> {
    pub fn assign<'c>(
        &'c mut self,
        location: BasicBlock,
        dest_ref: Local,
    ) -> RuntimeCallAdderForAssignment<'tcx, 'm, 'c> {
        RuntimeCallAdderForAssignment {
            call_adder: self,
            location,
            dest_ref,
        }
    }
}

impl<'tcx, 'm> RuntimeCallAdder<'tcx, 'm> {
    fn add_bb_for_call(&mut self, location: BasicBlock, func_name: &str, args: Vec<Operand<'tcx>>) {
        let block = self.make_bb_for_call(func_name, args);
        self.modification_unit
            .insert_blocks_before(location, [block]);
    }

    fn make_bb_for_call(
        &mut self,
        func_name: &str,
        args: Vec<Operand<'tcx>>,
    ) -> BasicBlockData<'tcx> {
        self.make_bb_for_call_with_ret(func_name, args).0
    }

    fn make_bb_for_call_with_ret(
        &mut self,
        func_name: &str,
        args: Vec<Operand<'tcx>>,
    ) -> (BasicBlockData<'tcx>, Local) {
        let result_local = self
            .modification_unit
            .add_local(self.get_pri_return_ty(func_name));

        (
            self.make_call_bb(func_name, args, Place::from(result_local)),
            result_local,
        )
    }

    fn make_call_bb(
        &self,
        func_name: &str,
        args: Vec<Operand<'tcx>>,
        destination: Place<'tcx>,
    ) -> BasicBlockData<'tcx> {
        BasicBlockData::new(Some(self.make_call_terminator(
            func_name,
            args,
            destination,
        )))
    }

    fn make_call_terminator(
        &self,
        func_name: &str,
        args: Vec<Operand<'tcx>>,
        destination: Place<'tcx>,
    ) -> Terminator<'tcx> {
        Terminator {
            source_info: SourceInfo::outermost(DUMMY_SP),
            kind: TerminatorKind::Call {
                /*
                 * NOTE: Check if it is supposed to be the same operand for each function definition,
                 * i.e. caching/lazy singleton.
                 */
                func: Operand::function_handle(
                    self.tcx,
                    self.get_pri_func_def_id(func_name),
                    std::iter::empty(),
                    DUMMY_SP,
                ),
                args: args,
                destination: destination,
                target: Some(modification::NEXT_BLOCK),
                cleanup: None,
                from_hir_call: true,
                fn_span: DUMMY_SP,
            },
        }
    }

    fn get_pri_func_def_id(&self, func_name: &str) -> DefId {
        self.get_pri_func_info(func_name).def_id
    }

    fn get_pri_return_ty(&self, func_name: &str) -> Ty<'tcx> {
        self.get_pri_func_info(func_name).ret_ty
    }

    fn get_pri_func_info(&self, func_name: &str) -> &FunctionInfo<'tcx> {
        self.pri_functions
            .get(&func_name.replace(" ", "")) // FIXME
            .expect("Invalid pri function name.")
    }
}

pub struct RuntimeCallAdderForAssignment<'tcx, 'm, 'c> {
    pub call_adder: &'c mut RuntimeCallAdder<'tcx, 'm>,
    location: BasicBlock,
    dest_ref: Local,
}

impl<'tcx, 'm, 'c> RuntimeCallAdderForAssignment<'tcx, 'm, 'c> {
    pub fn by_use(&mut self, operand_ref: Local) {
        self.add_bb_for_assign_call(
            stringify!(pri::assign_use),
            vec![operand::copy_for_local(operand_ref)],
        )
    }

    pub fn by_repeat(&mut self, operand_ref: Local, count: u64) {
        self.add_bb_for_assign_call(
            stringify!(pri::assign_repeat),
            vec![
                operand::copy_for_local(operand_ref),
                operand::const_from_uint(self.call_adder.tcx, count),
            ],
        )
    }

    pub fn by_ref(&mut self, place_ref: Local, is_mutable: bool) {
        self.add_bb_for_assign_call(
            stringify!(pri::assign_ref),
            vec![
                operand::copy_for_local(place_ref),
                operand::const_from_bool(self.call_adder.tcx, is_mutable),
            ],
        )
    }

    pub fn by_thread_local_ref(&mut self) {
        todo!()
    }

    pub fn by_address_of(&mut self, place_ref: Local, is_mutable: bool) {
        self.add_bb_for_assign_call(
            stringify!(pri::assign_address_of),
            vec![
                operand::copy_for_local(place_ref),
                operand::const_from_bool(self.call_adder.tcx, is_mutable),
            ],
        )
    }

    pub fn by_len(&mut self, place_ref: Local) {
        self.add_bb_for_assign_call(
            stringify!(pri::assign_len),
            vec![operand::copy_for_local(place_ref)],
        )
    }

    pub fn by_cast_numeric(&mut self, operand_ref: Local, is_to_float: bool, size: u64) {
        self.add_bb_for_assign_call(
            stringify!(pri::assign_cast_numeric),
            vec![
                operand::copy_for_local(operand_ref),
                operand::const_from_bool(self.call_adder.tcx, is_to_float),
                operand::const_from_uint(self.call_adder.tcx, size),
            ],
        )
    }

    pub fn by_cast(&mut self, operand_ref: Local, is_to_float: bool, size: u64) {
        todo!()
    }

    pub fn by_binary_op(
        &mut self,
        operator: &BinOp,
        first_ref: Local,
        second_ref: Local,
        checked: bool,
    ) {
        let operator = convert_mir_binop_to_pri(operator);
        let (operator_local, additional_statements) =
            self.add_and_set_local_for_enum(self.call_adder.pri_special_types.binary_op, operator);

        self.add_bb_for_assign_call_with_statements(
            stringify!(pri::assign_binary_op),
            vec![
                operand::move_for_local(operator_local),
                operand::copy_for_local(first_ref),
                operand::copy_for_local(second_ref),
                operand::const_from_bool(self.call_adder.tcx, checked),
            ],
            additional_statements,
        )
    }

    pub fn by_unary_op(&mut self, operator: &UnOp, operand_ref: Local) {
        let operator = convert_mir_unop_to_pri(operator);
        let (operator_local, additional_statements) =
            self.add_and_set_local_for_enum(self.call_adder.pri_special_types.unary_op, operator);

        self.add_bb_for_assign_call_with_statements(
            stringify!(pri::assign_unary_op),
            vec![
                operand::move_for_local(operator_local),
                operand::copy_for_local(operand_ref),
            ],
            additional_statements,
        )
    }

    pub fn by_discriminant(&mut self, place_ref: Local) {
        self.add_bb_for_assign_call(
            stringify!(pri::assign_discriminant),
            vec![operand::copy_for_local(place_ref)],
        )
    }

    pub fn by_aggregate_array(&mut self, items: &[Local]) {
        let tcx = self.call_adder.tcx;
        let operand_ref_ty = self.call_adder.pri_special_types.operand_ref;
        let items_local = self
            .call_adder
            .modification_unit
            .add_local(tcx.mk_array(operand_ref_ty, items.len() as u64));
        assignment::array_of_locals_by_move(Place::from(items_local), operand_ref_ty, &items);
        let items_ref_local = self.call_adder.modification_unit.add_local(
            self.call_adder
                .tcx
                .mk_imm_ref(tcx.lifetimes.re_erased, operand_ref_ty),
        );
        assignment::ref_of(Place::from(items_ref_local), Place::from(items_local), tcx);

        self.add_bb_for_assign_call(
            stringify!(pri::assign_binary_op),
            vec![operand::move_for_local(items_ref_local)],
        )
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
        self.call_adder
            .modification_unit
            .insert_blocks_before(self.location, [block]);
    }

    fn make_bb_for_assign_call(
        &mut self,
        func_name: &str,
        args: Vec<Operand<'tcx>>,
    ) -> BasicBlockData<'tcx> {
        self.call_adder.make_bb_for_call(
            func_name,
            [vec![operand::copy_for_local(self.dest_ref)], args].concat(),
        )
    }

    fn add_and_set_local_for_enum<T>(
        &mut self,
        enum_ty: Ty<'tcx>,
        value: T,
    ) -> (Local, Vec<Statement<'tcx>>)
    where
        T: Debug,
    {
        let local = self.call_adder.modification_unit.add_local(enum_ty);
        let statements =
            enums::set_variant_to_local(enum_ty, format!("{:?}", value).as_str(), local);
        (local, statements)
    }
}

mod utils {
    use rustc_middle::mir;

    pub mod operand {

        use std::mem::size_of;

        use rustc_const_eval::interpret::Scalar;
        use rustc_middle::{
            mir::{Local, Operand, Place},
            ty::{ScalarInt, Ty, TyCtxt},
        };
        use rustc_span::DUMMY_SP;
        use rustc_type_ir::UintTy;

        pub fn const_from_uint<'tcx, T>(tcx: TyCtxt<'tcx>, value: T) -> Operand<'tcx>
        where
            T: Into<u128>,
        {
            const_from_scalar_int(
                tcx,
                ScalarInt::try_from_uint(value, rustc_abi::Size::from_bytes(size_of::<T>()))
                    .unwrap(),
                tcx.mk_mach_uint(
                    [
                        UintTy::U8,
                        UintTy::U16,
                        UintTy::U32,
                        UintTy::U64,
                        UintTy::U128,
                    ]
                    .into_iter()
                    .find(|t| (t.bit_width().unwrap() / 8) as usize == size_of::<T>())
                    .unwrap(),
                ),
            )
        }

        pub fn const_from_bool<'tcx>(tcx: TyCtxt<'tcx>, value: bool) -> Operand<'tcx> {
            const_from_scalar_int(tcx, ScalarInt::from(value), tcx.types.bool)
        }

        pub fn const_from_scalar_int<'tcx>(
            tcx: TyCtxt<'tcx>,
            value: ScalarInt,
            ty: Ty<'tcx>,
        ) -> Operand<'tcx> {
            Operand::const_from_scalar(tcx, ty, Scalar::Int(value), DUMMY_SP)
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
                Operand::Move(Place::from(place))
            }
        }
    }

    pub mod enums {
        use rustc_middle::{
            mir::{Local, Place, SourceInfo, Statement},
            ty::{Ty, TyKind},
        };
        use rustc_span::DUMMY_SP;
        use rustc_target::abi::VariantIdx;

        pub fn set_variant_to_local<'tcx>(
            enum_ty: Ty<'tcx>,
            variant_name: &str,
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
                    variant_index: get_variant_index_by_name(enum_ty, variant_name),
                },
            };

            vec![deinit, disc]
        }

        pub fn get_variant_index_by_name<'tcx>(ty: Ty<'tcx>, variant_name: &str) -> VariantIdx {
            let adt_def = match ty.kind() {
                TyKind::Adt(def, _) => def,
                _ => unreachable!(),
            };
            let y = adt_def
                .variants()
                .iter()
                .find(|v| v.name.as_str() == variant_name)
                .expect(
                    format!("Variant could not be found with name `{}`.", variant_name).as_str(),
                );
            adt_def.variant_index_with_ctor_id(y.def_id)
        }
    }

    pub mod assignment {
        use rustc_middle::{
            mir::{
                AggregateKind, BorrowKind, Local, Operand, Place, Rvalue, SourceInfo, Statement,
                StatementKind,
            },
            ty::{Ty, TyCtxt},
        };
        use rustc_span::DUMMY_SP;

        use super::operand;

        pub fn ref_of<'tcx>(
            destination: Place<'tcx>,
            target: Place<'tcx>,
            tcx: TyCtxt<'tcx>,
        ) -> Statement<'tcx> {
            rvalue(
                destination,
                Rvalue::Ref(tcx.lifetimes.re_erased, BorrowKind::Shared, target),
            )
        }

        pub fn array_of_locals_by_move<'tcx>(
            destination: Place<'tcx>,
            ty: Ty<'tcx>,
            items: &[Local],
        ) -> Statement<'tcx> {
            array(
                destination,
                ty,
                Vec::from_iter(items.iter().map(|l| operand::move_for_local(l.clone()))),
            )
        }

        pub fn array<'tcx>(
            destination: Place<'tcx>,
            ty: Ty<'tcx>,
            items: Vec<Operand<'tcx>>,
        ) -> Statement<'tcx> {
            rvalue(
                destination,
                Rvalue::Aggregate(Box::new(AggregateKind::Array(ty)), items),
            )
        }

        pub fn rvalue<'tcx>(destination: Place<'tcx>, value: Rvalue<'tcx>) -> Statement<'tcx> {
            Statement {
                source_info: SourceInfo::outermost(DUMMY_SP),
                kind: StatementKind::Assign(Box::new((destination, value))),
            }
        }
    }

    pub fn convert_mir_binop_to_pri(op: &mir::BinOp) -> pri::BinaryOp {
        match op {
            mir::BinOp::Add => pri::BinaryOp::Add,
            mir::BinOp::Sub => pri::BinaryOp::Sub,
            mir::BinOp::Mul => pri::BinaryOp::Mul,
            mir::BinOp::Div => pri::BinaryOp::Div,
            mir::BinOp::Rem => pri::BinaryOp::Rem,
            mir::BinOp::BitXor => pri::BinaryOp::BitXor,
            mir::BinOp::BitAnd => pri::BinaryOp::BitAnd,
            mir::BinOp::BitOr => pri::BinaryOp::BitOr,
            mir::BinOp::Shl => pri::BinaryOp::Shl,
            mir::BinOp::Shr => pri::BinaryOp::Shr,
            mir::BinOp::Eq => pri::BinaryOp::Eq,
            mir::BinOp::Lt => pri::BinaryOp::Lt,
            mir::BinOp::Le => pri::BinaryOp::Le,
            mir::BinOp::Ne => pri::BinaryOp::Ne,
            mir::BinOp::Ge => pri::BinaryOp::Ge,
            mir::BinOp::Gt => pri::BinaryOp::Gt,
            mir::BinOp::Offset => pri::BinaryOp::Offset,
        }
    }

    pub fn convert_mir_unop_to_pri(op: &mir::UnOp) -> pri::UnaryOp {
        match op {
            mir::UnOp::Not => pri::UnaryOp::Not,
            mir::UnOp::Neg => pri::UnaryOp::Neg,
        }
    }
}
