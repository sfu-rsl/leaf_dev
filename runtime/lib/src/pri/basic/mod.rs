mod ffi;
mod instance;
mod utils;

use common::{log_debug, log_info, pri::*};

use self::instance::*;
use crate::abs::{
    self, backend::*, AssertKind, BranchingMetadata, CastKind, FloatType, IntType, Local, ValueType,
};
use common::log_warn;
use leaf_macros::trait_log_fn;

pub struct BasicPri;

const TAG: &str = "pri";

#[trait_log_fn(target = "pri", level = "debug")]
impl ProgramRuntimeInterface for BasicPri {
    type U128 = u128;
    type Char = char;
    type ConstStr = &'static str;
    type ConstByteStr = &'static [u8];
    type Slice<'a, T: 'a> = &'a [T];
    type BranchingInfo = BranchingInfo;
    type TypeId = TypeId;
    type BinaryOp = abs::BinaryOp;
    type UnaryOp = abs::UnaryOp;
    type DebugInfo = DebugInfo;

    fn init_runtime_lib() {
        init_backend();
    }

    fn shutdown_runtime_lib() {
        log_warn!("Shutting down has no effect on the basic backend.");
    }

    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn ref_place_return_value() -> PlaceRef {
        push_place_ref(|p| p.of_local(Local::ReturnValue))
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn ref_place_argument(local_index: LocalIndex) -> PlaceRef {
        push_place_ref(|p| p.of_local(Local::Argument(local_index)))
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn ref_place_local(local_index: LocalIndex) -> PlaceRef {
        push_place_ref(|p| p.of_local(Local::Normal(local_index)))
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn ref_place_deref(place: PlaceRef) {
        mut_place_ref(place, |p, place| p.project_on(place).deref())
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn ref_place_field(place: PlaceRef, field: FieldIndex /*, type */) {
        mut_place_ref(place, |p, place| p.project_on(place).for_field(field))
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn ref_place_index(place: PlaceRef, index_place: PlaceRef) {
        let index = take_back_place_ref(index_place)
            .try_into()
            .unwrap_or_else(|p| {
                panic!("The place used as an index should be just a local. {:?}", p)
            });
        mut_place_ref(place, |p, place| p.project_on(place).at_index(index))
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn ref_place_constant_index(place: PlaceRef, offset: u64, min_length: u64, from_end: bool) {
        mut_place_ref(place, |p, place| {
            p.project_on(place)
                .at_constant_index(offset, min_length, from_end)
        })
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn ref_place_subslice(place: PlaceRef, from: u64, to: u64, from_end: bool) {
        mut_place_ref(place, |p, place| {
            p.project_on(place).subslice(from, to, from_end)
        })
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn ref_place_downcast(place: PlaceRef, variant_index: u32 /*, type */) {
        mut_place_ref(place, |p, place| {
            p.project_on(place).downcast(variant_index)
        })
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn ref_place_opaque_cast(place: PlaceRef /*, type */) {
        mut_place_ref(place, |p, place| p.project_on(place).opaque_cast())
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn ref_place_subtype(place: PlaceRef /*, type */) {
        mut_place_ref(place, |p, place| p.project_on(place).subtype())
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn set_place_address(place: PlaceRef, raw_ptr: RawPointer) {
        mut_place_ref(place, |p, place| p.metadata(place).set_address(raw_ptr));
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn set_place_type_id(place: PlaceRef, type_id: Self::TypeId) {
        mut_place_ref(place, |h, p| h.metadata(p).set_type_id(type_id))
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn set_place_type_bool(place: PlaceRef) {
        Self::set_place_type(place, ValueType::Bool)
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn set_place_type_char(place: PlaceRef) {
        Self::set_place_type(place, ValueType::Char)
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn set_place_type_int(place: PlaceRef, bit_size: u64, is_signed: bool) {
        Self::set_place_type(place, ValueType::new_int(bit_size, is_signed))
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn set_place_type_float(place: PlaceRef, e_bits: u64, s_bits: u64) {
        Self::set_place_type(place, ValueType::new_float(e_bits, s_bits))
    }
    #[tracing::instrument(target = "pri::place", level = "debug", ret)]
    fn set_place_size(place: PlaceRef, byte_size: TypeSize) {
        mut_place_ref(place, |h, p| h.metadata(p).set_size(byte_size))
    }

    #[tracing::instrument(target = "pri::operand", level = "debug", ret)]
    fn ref_operand_copy(place: PlaceRef) -> OperandRef {
        push_operand_ref(|o| o.copy_of(take_back_place_ref(place)))
    }
    #[tracing::instrument(target = "pri::operand", level = "debug", ret)]
    fn ref_operand_move(place: PlaceRef) -> OperandRef {
        push_operand_ref(|o| o.move_of(take_back_place_ref(place)))
    }

    #[tracing::instrument(target = "pri::operand", level = "debug", ret)]
    fn ref_operand_const_bool(value: bool) -> OperandRef {
        push_operand_ref(|o| o.const_from().bool(value))
    }
    #[tracing::instrument(target = "pri::operand", level = "debug", ret)]
    fn ref_operand_const_int(bit_rep: u128, bit_size: u64, is_signed: bool) -> OperandRef {
        push_operand_ref(|o| {
            o.const_from().int(
                bit_rep,
                IntType {
                    bit_size,
                    is_signed,
                },
            )
        })
    }
    #[tracing::instrument(target = "pri::operand", level = "debug", ret)]
    fn ref_operand_const_float(bit_rep: u128, e_bits: u64, s_bits: u64) -> OperandRef {
        push_operand_ref(|o| o.const_from().float(bit_rep, FloatType { e_bits, s_bits }))
    }
    #[tracing::instrument(target = "pri::operand", level = "debug", ret)]
    fn ref_operand_const_char(value: char) -> OperandRef {
        push_operand_ref(|o| o.const_from().char(value))
    }
    #[tracing::instrument(target = "pri::operand", level = "debug", ret)]
    fn ref_operand_const_func(id: FuncId) -> OperandRef {
        push_operand_ref(|o| o.const_from().func(id))
    }
    #[tracing::instrument(target = "pri::operand", level = "debug", ret)]
    fn ref_operand_const_str(value: &'static str) -> OperandRef {
        push_operand_ref(|o| o.const_from().str(value))
    }
    #[tracing::instrument(target = "pri::operand", level = "debug", ret)]
    fn ref_operand_const_byte_str(value: &'static [u8]) -> OperandRef {
        push_operand_ref(|o| o.const_from().byte_str(value))
    }
    #[tracing::instrument(target = "pri::operand", level = "debug", ret)]
    fn ref_operand_const_zst() -> OperandRef {
        push_operand_ref(|o| o.const_from().zst())
    }
    #[tracing::instrument(target = "pri::operand", level = "debug", ret)]
    fn ref_operand_const_some() -> OperandRef {
        push_operand_ref(|o| o.const_from().some())
    }

    fn new_sym_value_bool(conc_val: bool) -> OperandRef {
        // FIXME: Redundant referencing.
        let conc_val = take_back_operand_ref(Self::ref_operand_const_bool(conc_val));
        push_operand_ref(|o| o.new_symbolic(conc_val, ValueType::Bool))
    }
    fn new_sym_value_char(conc_val: char) -> OperandRef {
        // FIXME: Redundant referencing.
        let conc_val = take_back_operand_ref(Self::ref_operand_const_char(conc_val));
        push_operand_ref(|o| o.new_symbolic(conc_val, ValueType::Char))
    }
    fn new_sym_value_int(conc_val_bit_rep: u128, bit_size: u64, is_signed: bool) -> OperandRef {
        // FIXME: Redundant referencing.
        let conc_val = take_back_operand_ref(Self::ref_operand_const_int(
            conc_val_bit_rep,
            bit_size,
            is_signed,
        ));
        push_operand_ref(|o| o.new_symbolic(conc_val, ValueType::new_int(bit_size, is_signed)))
    }
    fn new_sym_value_float(conc_val_bit_rep: u128, e_bits: u64, s_bits: u64) -> OperandRef {
        // FIXME: Redundant referencing.
        let conc_val = take_back_operand_ref(Self::ref_operand_const_float(
            conc_val_bit_rep,
            e_bits,
            s_bits,
        ));
        push_operand_ref(|o| o.new_symbolic(conc_val, ValueType::new_float(e_bits, s_bits)))
    }

    fn assign_use(dest: PlaceRef, operand: OperandRef) {
        assign_to(dest, |h| h.use_of(take_back_operand_ref(operand)))
    }
    fn assign_repeat(dest: PlaceRef, operand: OperandRef, count: usize) {
        assign_to(dest, |h| h.repeat_of(take_back_operand_ref(operand), count))
    }
    fn assign_ref(dest: PlaceRef, place: PlaceRef, is_mutable: bool) {
        assign_to(dest, |h| h.ref_to(take_back_place_ref(place), is_mutable))
    }
    fn assign_thread_local_ref(dest: PlaceRef /* TODO: #365 */) {
        assign_to(dest, |h| h.thread_local_ref_to())
    }
    fn assign_raw_ptr_of(dest: PlaceRef, place: PlaceRef, is_mutable: bool) {
        assign_to(dest, |h| {
            h.address_of(take_back_place_ref(place), is_mutable)
        })
    }
    fn assign_len(dest: PlaceRef, place: PlaceRef) {
        // To be investigated. Not obvious whether it appears at all in the later stages.
        assign_to(dest, |h| h.len_of(take_back_place_ref(place)))
    }

    fn assign_cast_char(dest: PlaceRef, operand: OperandRef) {
        assign_to(dest, |h| {
            h.cast_of(take_back_operand_ref(operand), CastKind::ToChar)
        })
    }
    fn assign_cast_integer(dest: PlaceRef, operand: OperandRef, bit_size: u64, is_signed: bool) {
        assign_to(dest, |h| {
            h.cast_of(
                take_back_operand_ref(operand),
                CastKind::ToInt(IntType {
                    bit_size,
                    is_signed,
                }),
            )
        })
    }
    fn assign_cast_float(dest: PlaceRef, operand: OperandRef, e_bits: u64, s_bits: u64) {
        assign_to(dest, |h| {
            h.cast_of(
                take_back_operand_ref(operand),
                CastKind::ToFloat(FloatType { e_bits, s_bits }),
            )
        })
    }
    fn assign_cast_expose_prov(dest: PlaceRef, operand: OperandRef) {
        assign_to(dest, |h| {
            h.cast_of(take_back_operand_ref(operand), CastKind::ExposeProvenance)
        })
    }
    fn assign_cast_with_exposed_prov(dest: PlaceRef, operand: OperandRef, dst_type_id: TypeId) {
        Self::assign_cast_pointer(dest, operand, dst_type_id);
    }
    fn assign_cast_to_another_ptr(dest: PlaceRef, operand: OperandRef, dst_type_id: TypeId) {
        Self::assign_cast_pointer(dest, operand, dst_type_id);
    }

    fn assign_cast_unsize(dest: PlaceRef, operand: OperandRef) {
        assign_to(dest, |h| {
            h.cast_of(take_back_operand_ref(operand), CastKind::PointerUnsize)
        })
    }
    fn assign_cast_sized_dyn(dest: PlaceRef, operand: OperandRef) {
        assign_to(dest, |h| {
            h.cast_of(take_back_operand_ref(operand), CastKind::SizedDynamize)
        })
    }
    fn assign_cast_transmute(dest: PlaceRef, operand: OperandRef, dst_type_id: Self::TypeId) {
        assign_to(dest, |h| {
            h.cast_of(
                take_back_operand_ref(operand),
                CastKind::Transmute(dst_type_id),
            )
        })
    }

    fn assign_binary_op(
        dest: PlaceRef,
        operator: Self::BinaryOp,
        first: OperandRef,
        second: OperandRef,
    ) {
        assign_to(dest, |h| {
            h.binary_op_between(
                operator,
                take_back_operand_ref(first),
                take_back_operand_ref(second),
            )
        })
    }
    fn assign_unary_op(dest: PlaceRef, operator: Self::UnaryOp, operand: OperandRef) {
        assign_to(dest, |h| {
            h.unary_op_on(operator, take_back_operand_ref(operand))
        })
    }

    fn set_discriminant(dest: PlaceRef, variant_index: u32) {
        assign_to(dest, |h| h.variant_index(variant_index))
    }
    fn assign_discriminant(dest: PlaceRef, place: PlaceRef) {
        assign_to(dest, |h| h.discriminant_of(take_back_place_ref(place)))
    }

    // We use slice to simplify working with the interface.
    fn assign_aggregate_array(dest: PlaceRef, items: &[OperandRef]) {
        assign_to(dest, |h| {
            h.array_from(items.iter().map(|o| take_back_operand_ref(*o)))
        })
    }
    fn assign_aggregate_tuple(dest: PlaceRef, fields: &[OperandRef]) {
        assign_to(dest, |h| {
            let fields = Self::take_fields(fields);
            h.tuple_from(fields.into_iter())
        })
    }
    fn assign_aggregate_struct(dest: PlaceRef, fields: &[OperandRef]) {
        assign_to(dest, |h| {
            let fields = Self::take_fields(fields);
            h.adt_from(fields.into_iter(), None)
        })
    }
    fn assign_aggregate_enum(dest: PlaceRef, fields: &[OperandRef], variant: VariantIndex) {
        assign_to(dest, |h| {
            let fields = Self::take_fields(fields);
            h.adt_from(fields.into_iter(), Some(variant))
        })
    }
    fn assign_aggregate_union(dest: PlaceRef, active_field: FieldIndex, value: OperandRef) {
        assign_to(dest, |h| {
            let field = Self::take_fields(&[value]).pop().unwrap();
            h.union_from(active_field, field)
        })
    }
    fn assign_aggregate_closure(dest: PlaceRef, upvars: &[OperandRef]) {
        assign_to(dest, |h| {
            let upvars = Self::take_fields(upvars);
            h.closure_from(upvars.into_iter())
        })
    }
    fn assign_aggregate_coroutine(dest: PlaceRef, upvars: &[OperandRef]) {
        assign_to(dest, |h| {
            let upvars = Self::take_fields(upvars);
            h.coroutine_from(upvars.into_iter())
        })
    }
    fn assign_aggregate_coroutine_closure(dest: PlaceRef, upvars: &[OperandRef]) {
        assign_to(dest, |h| {
            let upvars = Self::take_fields(upvars);
            h.coroutine_closure_from(upvars.into_iter())
        })
    }
    fn assign_aggregate_raw_ptr(
        dest: PlaceRef,
        data_ptr: OperandRef,
        metadata: OperandRef,
        is_mutable: bool,
    ) {
        assign_to(dest, |h| {
            h.raw_ptr_from(
                take_back_operand_ref(data_ptr),
                take_back_operand_ref(metadata),
                is_mutable,
            )
        })
    }

    fn assign_shallow_init_box(dest: PlaceRef, operand: OperandRef, _boxed_type_id: Self::TypeId) {
        assign_to(dest, |h| {
            h.shallow_init_box_from(take_back_operand_ref(operand))
        })
    }

    fn new_branching_info(
        node_location: BasicBlockIndex,
        discriminant: OperandRef,
        discr_bit_size: u64,
        discr_is_signed: bool,
    ) -> BranchingInfo {
        BranchingInfo::new(node_location, discriminant, discr_bit_size, discr_is_signed)
    }
    fn take_branch_true(info: BranchingInfo) {
        conditional(info, |h| h.on_bool().take(true))
    }
    fn take_branch_false(info: BranchingInfo) {
        conditional(info, |h| h.on_bool().take(false))
    }

    fn take_branch_int(info: BranchingInfo, value_bit_rep: u128) {
        conditional(info, |h| h.on_int().take(value_bit_rep))
    }
    fn take_branch_ow_int(info: BranchingInfo, non_values: &[u128]) {
        conditional(info, |h| h.on_int().take_otherwise(non_values))
    }

    fn take_branch_char(info: BranchingInfo, value: char) {
        conditional(info, |h| h.on_char().take(value))
    }
    fn take_branch_ow_char(info: BranchingInfo, non_values: &[char]) {
        conditional(info, |h| h.on_char().take_otherwise(non_values))
    }

    fn take_branch_enum_discriminant(info: BranchingInfo, index: VariantIndex) {
        conditional(info, |h| h.on_enum().take(index))
    }
    fn take_branch_ow_enum_discriminant(info: BranchingInfo, non_indices: &[VariantIndex]) {
        conditional(info, |h| h.on_enum().take_otherwise(non_indices))
    }

    #[tracing::instrument(target = "pri::call", level = "debug")]
    fn before_call_func(func: OperandRef, args: &[OperandRef], are_args_tupled: bool) {
        func_control(|h| {
            h.before_call(
                take_back_operand_ref(func),
                args.iter().map(|o| take_back_operand_ref(*o)),
                are_args_tupled,
            )
        });
    }
    #[tracing::instrument(target = "pri::place", level = "debug")]
    fn preserve_special_local_metadata(place: PlaceRef) {
        func_control(|h| h.metadata().preserve_metadata(take_back_place_ref(place)))
    }
    #[tracing::instrument(target = "pri::call", level = "debug")]
    fn try_untuple_argument(arg_index: LocalIndex, tuple_type_id: TypeId) {
        func_control(|h| h.metadata().try_untuple_argument(arg_index, tuple_type_id))
    }
    #[tracing::instrument(target = "pri::call", level = "debug")]
    fn enter_func(func: OperandRef) {
        func_control(|h| h.enter(take_back_operand_ref(func)))
    }
    #[tracing::instrument(target = "pri::call", level = "debug")]
    fn return_from_func() {
        func_control(|h| h.ret())
    }
    /// Overrides (forces) the return value of a function.
    /// In an external call chain, the value will be kept as the return value
    /// until it is consumed at the point of return to an internal caller.
    #[tracing::instrument(target = "pri::call", level = "debug")]
    fn override_return_value(operand: OperandRef) {
        func_control(|h| h.override_return_value(take_back_operand_ref(operand)))
    }
    #[tracing::instrument(target = "pri::call", level = "debug")]
    fn after_call_func(destination: PlaceRef) {
        func_control(|h| h.after_call(take_back_place_ref(destination)))
    }

    fn check_assert_bounds_check(
        cond: OperandRef,
        expected: bool,
        len: OperandRef,
        index: OperandRef,
    ) {
        let assert_kind = AssertKind::BoundsCheck {
            len: take_back_operand_ref(len),
            index: take_back_operand_ref(index),
        };
        Self::check_assert(cond, expected, assert_kind)
    }
    fn check_assert_overflow(
        cond: OperandRef,
        expected: bool,
        operator: Self::BinaryOp,
        first: OperandRef,
        second: OperandRef,
    ) {
        let assert_kind = AssertKind::Overflow(
            operator,
            take_back_operand_ref(first),
            take_back_operand_ref(second),
        );
        Self::check_assert(cond, expected, assert_kind)
    }
    fn check_assert_overflow_neg(cond: OperandRef, expected: bool, operand: OperandRef) {
        let assert_kind = AssertKind::OverflowNeg(take_back_operand_ref(operand));
        Self::check_assert(cond, expected, assert_kind)
    }
    fn check_assert_div_by_zero(cond: OperandRef, expected: bool, operand: OperandRef) {
        let assert_kind = AssertKind::DivisionByZero(take_back_operand_ref(operand));
        Self::check_assert(cond, expected, assert_kind)
    }
    fn check_assert_rem_by_zero(cond: OperandRef, expected: bool, operand: OperandRef) {
        let assert_kind = AssertKind::RemainderByZero(take_back_operand_ref(operand));
        Self::check_assert(cond, expected, assert_kind)
    }
    fn check_assert_misaligned_ptr_deref(
        cond: OperandRef,
        expected: bool,
        required: OperandRef,
        found: OperandRef,
    ) {
        let assert_kind = AssertKind::MisalignedPointerDereference {
            required: take_back_operand_ref(required),
            found: take_back_operand_ref(found),
        };
        Self::check_assert(cond, expected, assert_kind)
    }

    #[tracing::instrument(target = "pri", skip_all, level = "trace")]
    fn debug_info(info: Self::DebugInfo) {
        let str_rep = String::from_utf8_lossy(info);
        let str_rep = str_rep.trim_matches('"');
        const MAX_LEN: usize = 120;
        if str_rep.len() <= MAX_LEN {
            log_info!(target: TAG, "{}", str_rep);
        } else {
            log_info!(target: TAG, "{}…", &str_rep[..MAX_LEN]);
            log_debug!(target: TAG, "Full debug info: {}", str_rep);
        }
    }

    fn intrinsic_assign_rotate_left(dest: PlaceRef, x: OperandRef, shift: OperandRef) {
        Self::assign_binary_op(dest, Self::BinaryOp::RotateL, x, shift)
    }

    fn intrinsic_assign_rotate_right(dest: PlaceRef, x: OperandRef, shift: OperandRef) {
        Self::assign_binary_op(dest, Self::BinaryOp::RotateR, x, shift)
    }

    fn intrinsic_assign_saturating_add(dest: PlaceRef, first: OperandRef, second: OperandRef) {
        Self::assign_binary_op(dest, Self::BinaryOp::AddSaturating, first, second)
    }

    fn intrinsic_assign_saturating_sub(dest: PlaceRef, first: OperandRef, second: OperandRef) {
        Self::assign_binary_op(dest, Self::BinaryOp::SubSaturating, first, second)
    }

    fn intrinsic_assign_exact_div(dest: PlaceRef, first: OperandRef, second: OperandRef) {
        Self::assign_binary_op(dest, Self::BinaryOp::DivExact, first, second);
    }

    fn intrinsic_assign_bitreverse(dest: PlaceRef, x: OperandRef) {
        Self::assign_unary_op(dest, Self::UnaryOp::BitReverse, x);
    }

    fn intrinsic_assign_cttz_nonzero(dest: PlaceRef, x: OperandRef) {
        todo!("Implement cttz_nonzero intrinsic");
    }

    fn intrinsic_assign_cttz(dest: PlaceRef, x: OperandRef) {
        todo!("Implement cttz intrinsic");
    }

    fn intrinsic_assign_ctpop(dest: PlaceRef, x: OperandRef) {
        todo!("Implement ctpop intrinsic");
    }

    fn intrinsic_assign_ctlz_nonzero(dest: PlaceRef, x: OperandRef) {
        todo!("Implement ctlz_nonzero intrinsic");
    }

    fn intrinsic_assign_ctlz(dest: PlaceRef, x: OperandRef) {
        todo!("Implement ctlz intrinsic");
    }
}

impl BasicPri {
    fn set_place_type(place: PlaceRef, ty: ValueType) {
        mut_place_ref(place, |p, place| p.metadata(place).set_primitive_type(ty));
    }

    fn take_fields(fields: &[OperandRef]) -> Vec<FieldImpl> {
        let fields = fields.iter().map(|o| take_back_operand_ref(*o));
        fields.map(Into::<FieldImpl>::into).collect()
    }

    fn assign_cast_pointer(dest: PlaceRef, operand: OperandRef, dst_type_id: TypeId) {
        assign_to(dest, |h| {
            h.cast_of(
                take_back_operand_ref(operand),
                CastKind::ToPointer(dst_type_id),
            )
        })
    }

    fn check_assert(cond: OperandRef, expected: bool, assert_kind: AssertKind<OperandImpl>) {
        branch(|h| h.assert(take_back_operand_ref(cond), expected, assert_kind))
    }
}

pub struct BranchingInfo {
    pub discriminant: OperandRef,
    pub(crate) metadata: BranchingMetadata,
}

impl BranchingInfo {
    #[inline]
    pub(crate) fn new(
        node_location: BasicBlockIndex,
        discriminant: OperandRef,
        discr_bit_size: u64,
        discr_is_signed: bool,
    ) -> Self {
        Self {
            discriminant,
            metadata: BranchingMetadata {
                node_location,
                discr_as_int: IntType {
                    bit_size: discr_bit_size,
                    is_signed: discr_is_signed,
                },
            },
        }
    }
}

impl core::fmt::Debug for BranchingInfo {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "BranchingInfo(on: {} as {} @ {})",
            self.discriminant, self.metadata.discr_as_int, self.metadata.node_location
        )
    }
}
