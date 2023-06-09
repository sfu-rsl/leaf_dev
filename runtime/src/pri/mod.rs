mod instance;
mod utils;

use crate::abs::{
    backend::*, AssertKind, BasicBlockIndex, BinaryOp, BranchingMetadata, DiscriminantAsIntType,
    FieldIndex, Local, LocalIndex, UnaryOp, VariantIndex,
};

use self::instance::*;

pub type Ref = u64;
pub type PlaceRef = Ref;
pub type OperandRef = Ref;

/*
 * This field serves as a marker to find the module in the compiler easier.
 */
pub static MODULE_MARKER: u8 = 0;

/*
 * These fields serve as exported symbols to get the types of the desired
 * arguments easier in the compiler.
 */
pub static PLACE_REF_TYPE_HOLDER: PlaceRef = 0;
pub static OPERAND_REF_TYPE_HOLDER: OperandRef = 0;
pub static BINARY_OP_TYPE_HOLDER: BinaryOp = BinaryOp::Add;
pub static UNARY_OP_TYPE_HOLDER: UnaryOp = UnaryOp::Neg;

pub fn init_runtime_lib() {
    init_backend()
}

pub fn ref_place_return_value() -> PlaceRef {
    push_place_ref(|p| p.of_local(Local::ReturnValue))
}
pub fn ref_place_argument(local_index: LocalIndex) -> PlaceRef {
    push_place_ref(|p| p.of_local(Local::Argument(local_index)))
}
pub fn ref_place_local(local_index: LocalIndex) -> PlaceRef {
    push_place_ref(|p| p.of_local(Local::Normal(local_index)))
}

pub fn ref_place_deref(place: PlaceRef) -> PlaceRef {
    push_place_ref(|p| p.project_on(take_back_place_ref(place)).deref())
}
pub fn ref_place_field(place: PlaceRef, field: FieldIndex /*, type */) -> PlaceRef {
    push_place_ref(|p| p.project_on(take_back_place_ref(place)).for_field(field))
}
pub fn ref_place_index(place: PlaceRef, index_place: PlaceRef) -> PlaceRef {
    push_place_ref(|p| {
        p.project_on(take_back_place_ref(place))
            .at_index(take_back_place_ref(index_place))
    })
}
pub fn ref_place_constant_index(
    place: PlaceRef,
    offset: u64,
    min_length: u64,
    from_end: bool,
) -> PlaceRef {
    push_place_ref(|p| {
        p.project_on(take_back_place_ref(place))
            .at_constant_index(offset, min_length, from_end)
    })
}
pub fn ref_place_subslice(place: PlaceRef, from: u64, to: u64, from_end: bool) -> PlaceRef {
    push_place_ref(|p| {
        p.project_on(take_back_place_ref(place))
            .subslice(from, to, from_end)
    })
}
pub fn ref_place_downcast(place: PlaceRef, variant_index: u32 /*, type */) -> PlaceRef {
    push_place_ref(|p| {
        p.project_on(take_back_place_ref(place))
            .downcast(variant_index)
    })
}
pub fn ref_place_opaque_cast(place: PlaceRef /*, type */) -> PlaceRef {
    push_place_ref(|p| p.project_on(take_back_place_ref(place)).opaque_cast())
}

pub fn ref_operand_copy(place: PlaceRef) -> OperandRef {
    push_operand_ref(|o| o.copy_of(take_back_place_ref(place)))
}
pub fn ref_operand_move(place: PlaceRef) -> OperandRef {
    push_operand_ref(|o| o.move_of(take_back_place_ref(place)))
}

pub fn ref_operand_const_bool(value: bool) -> OperandRef {
    push_operand_ref(|o| o.const_from().bool(value))
}
pub fn ref_operand_const_int(bit_rep: u128, size: u64, is_signed: bool) -> OperandRef {
    push_operand_ref(|o| o.const_from().int(bit_rep, size, is_signed))
}
pub fn ref_operand_const_float(bit_rep: u128, ebits: u64, sbits: u64) -> OperandRef {
    push_operand_ref(|o| o.const_from().float(bit_rep, ebits, sbits))
}
pub fn ref_operand_const_char(value: char) -> OperandRef {
    push_operand_ref(|o| o.const_from().char(value))
}
pub fn ref_operand_const_func(id: u64) -> OperandRef {
    push_operand_ref(|o| o.const_from().func(id))
}
pub fn ref_operand_const_str(value: &'static str) -> OperandRef {
    push_operand_ref(|o| o.const_from().str(value))
}

pub fn new_sym_value_bool() -> OperandRef {
    push_operand_ref(|o| o.new_symbolic().bool())
}
pub fn new_sym_value_char() -> OperandRef {
    push_operand_ref(|o| o.new_symbolic().char())
}
pub fn new_sym_value_int(size: u64, is_signed: bool) -> OperandRef {
    push_operand_ref(|o| o.new_symbolic().int(size, is_signed))
}
pub fn new_sym_value_float(ebits: u64, sbits: u64) -> OperandRef {
    push_operand_ref(|o| o.new_symbolic().float(ebits, sbits))
}

pub fn assign_use(dest: PlaceRef, operand: OperandRef) {
    assign_to(dest, |h| h.use_of(take_back_operand_ref(operand)))
}
pub fn assign_repeat(dest: PlaceRef, operand: OperandRef, count: usize) {
    assign_to(dest, |h| h.repeat_of(take_back_operand_ref(operand), count))
}
pub fn assign_ref(dest: PlaceRef, place: PlaceRef, is_mutable: bool) {
    assign_to(dest, |h| h.ref_to(take_back_place_ref(place), is_mutable))
}
pub fn assign_thread_local_ref(
    dest: PlaceRef, /* TODO: Complicated. MIRAI has some works on it. */
) {
    assign_to(dest, |h| h.thread_local_ref_to())
}
pub fn assign_address_of(dest: PlaceRef, place: PlaceRef, is_mutable: bool) {
    assign_to(dest, |h| {
        h.address_of(take_back_place_ref(place), is_mutable)
    })
}
pub fn assign_len(dest: PlaceRef, place: PlaceRef) {
    // To be investigated. Not obvious whether it appears at all in the later stages.
    assign_to(dest, |h| h.len_of(take_back_place_ref(place)))
}

pub fn assign_cast_char(dest: PlaceRef, operand: OperandRef) {
    assign_to(dest, |h| h.char_cast_of(take_back_operand_ref(operand)))
}

pub fn assign_cast_integer(dest: PlaceRef, operand: OperandRef, is_signed: bool, bits: u64) {
    assign_to(dest, |h| {
        h.integer_cast_of(take_back_operand_ref(operand), is_signed, bits)
    })
}

pub fn assign_cast_float(dest: PlaceRef, operand: OperandRef, bits: u64) {
    assign_to(dest, |h| {
        h.float_cast_of(take_back_operand_ref(operand), bits)
    })
}

pub fn assign_cast(dest: PlaceRef /* TODO: Other types of cast. */) {
    assign_to(dest, |h| h.cast_of())
}

pub fn assign_binary_op(
    dest: PlaceRef,
    operator: BinaryOp,
    first: OperandRef,
    second: OperandRef,
    checked: bool,
) {
    assign_to(dest, |h| {
        h.binary_op_between(
            operator,
            take_back_operand_ref(first),
            take_back_operand_ref(second),
            checked,
        )
    })
}
pub fn assign_unary_op(dest: PlaceRef, operator: UnaryOp, operand: OperandRef) {
    assign_to(dest, |h| {
        h.unary_op_on(operator, take_back_operand_ref(operand))
    })
}

pub fn set_discriminant(dest: PlaceRef, variant_index: u32) {
    assign_to(dest, |h| h.variant_index(variant_index))
}
pub fn assign_discriminant(dest: PlaceRef, place: PlaceRef) {
    assign_to(dest, |h| h.discriminant_of(take_back_place_ref(place)))
}

// We use slice to simplify working with the interface.
pub fn assign_aggregate_array(dest: PlaceRef, items: &[OperandRef]) {
    assign_to(dest, |h| {
        h.array_from(items.iter().map(|o| take_back_operand_ref(*o)))
    })
}

pub fn take_branch_true(info: BranchingInfo) {
    conditional(info, |h| h.on_bool().take(true))
}
pub fn take_branch_false(info: BranchingInfo) {
    conditional(info, |h| h.on_bool().take(false))
}

pub fn take_branch_int(info: BranchingInfo, value_bit_rep: u128) {
    conditional(info, |h| h.on_int().take(value_bit_rep))
}
pub fn take_branch_ow_int(info: BranchingInfo, non_values: &[u128]) {
    conditional(info, |h| h.on_int().take_otherwise(non_values))
}

pub fn take_branch_char(info: BranchingInfo, value: char) {
    conditional(info, |h| h.on_char().take(value))
}
pub fn take_branch_ow_char(info: BranchingInfo, non_values: &[char]) {
    conditional(info, |h| h.on_char().take_otherwise(non_values))
}

pub fn take_branch_enum_discriminant(info: BranchingInfo, index: VariantIndex) {
    conditional(info, |h| h.on_enum().take(index))
}
pub fn take_branch_ow_enum_discriminant(info: BranchingInfo, non_indices: &[VariantIndex]) {
    conditional(info, |h| h.on_enum().take_otherwise(non_indices))
}

pub fn before_call_func(func: OperandRef, args: &[OperandRef]) {
    func_control(|h| {
        h.before_call(
            take_back_operand_ref(func),
            args.iter().map(|o| take_back_operand_ref(*o)),
        )
    });
}
pub fn enter_func() {
    func_control(|h| h.enter())
}
pub fn return_from_func() {
    func_control(|h| h.ret())
}
pub fn after_call_func(destination: PlaceRef) {
    func_control(|h| h.after_call(take_back_place_ref(destination)))
}

pub fn check_assert_bounds_check(
    cond: OperandRef,
    expected: bool,
    len: OperandRef,
    index: OperandRef,
) {
    let assert_kind = AssertKind::BoundsCheck {
        len: take_back_operand_ref(len),
        index: take_back_operand_ref(index),
    };
    check_assert(cond, expected, assert_kind)
}
pub fn check_assert_overflow(
    cond: OperandRef,
    expected: bool,
    operator: BinaryOp,
    first: OperandRef,
    second: OperandRef,
) {
    let assert_kind = AssertKind::Overflow(
        operator,
        take_back_operand_ref(first),
        take_back_operand_ref(second),
    );
    check_assert(cond, expected, assert_kind)
}
pub fn check_assert_overflow_neg(cond: OperandRef, expected: bool, operand: OperandRef) {
    let assert_kind = AssertKind::OverflowNeg(take_back_operand_ref(operand));
    check_assert(cond, expected, assert_kind)
}
pub fn check_assert_div_by_zero(cond: OperandRef, expected: bool, operand: OperandRef) {
    let assert_kind = AssertKind::DivisionByZero(take_back_operand_ref(operand));
    check_assert(cond, expected, assert_kind)
}
pub fn check_assert_rem_by_zero(cond: OperandRef, expected: bool, operand: OperandRef) {
    let assert_kind = AssertKind::RemainderByZero(take_back_operand_ref(operand));
    check_assert(cond, expected, assert_kind)
}
fn check_assert(cond: OperandRef, expected: bool, assert_kind: AssertKind<OperandImpl>) {
    branch(|h| h.assert(take_back_operand_ref(cond), expected, assert_kind))
}

pub struct BranchingInfo {
    pub discriminant: OperandRef,
    metadata: BranchingMetadata,
}

impl BranchingInfo {
    pub fn new(
        node_location: BasicBlockIndex,
        discriminant: OperandRef,
        discr_bit_size: u64,
        discr_is_signed: bool,
    ) -> Self {
        Self {
            discriminant,
            metadata: BranchingMetadata {
                node_location,
                discr_as_int: DiscriminantAsIntType {
                    bit_size: discr_bit_size,
                    is_signed: discr_is_signed,
                },
            },
        }
    }
}
