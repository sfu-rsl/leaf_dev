mod instance;
mod utils;

use crate::abs::{
    AssignmentHandler, BinaryOp, ConstantHandler, Local, OperandHandler, PlaceHandler,
    PlaceProjectionHandler, Runtime, UnaryOp,
};

use self::instance::*;

pub type Ref = u64;
pub type PlaceRef = Ref;
pub type OperandRef = Ref;

pub type VariantIndex = u32;

/*
 * These fields serve as exported symbols in the workaround to get the Tys for
 * the desired types in the compiler.
 */
pub static PLACE_REF_TYPE_HOLDER: PlaceRef = 0;
pub static OPERAND_REF_TYPE_HOLDER: OperandRef = 0;
pub static BINARY_OP_TYPE_HOLDER: BinaryOp = BinaryOp::Add;
pub static UNARY_OP_TYPE_HOLDER: UnaryOp = UnaryOp::Neg;

pub type BasicBlockIndex = u32;
pub type BranchTarget = BasicBlockIndex;

pub fn init_runtime() {
    todo!()
}

pub fn ref_place_local(local: Local) -> PlaceRef {
    push_place_ref(|mut p| p.of_local(local))
}

pub fn ref_place_deref(place: PlaceRef) -> PlaceRef {
    push_place_ref(|mut p| p.project_on(take_back_place_ref(place)).deref())
}
pub fn ref_place_field(place: PlaceRef, field: u32 /*, type */) -> PlaceRef {
    push_place_ref(|mut p| p.project_on(take_back_place_ref(place)).for_field(field))
}
pub fn ref_place_index(place: PlaceRef, index_place: PlaceRef) -> PlaceRef {
    push_place_ref(|mut p| {
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
    push_place_ref(|mut p| {
        p.project_on(take_back_place_ref(place))
            .at_constant_index(offset, min_length, from_end)
    })
}
pub fn ref_place_subslice(place: PlaceRef, from: u64, to: u64, from_end: bool) -> PlaceRef {
    push_place_ref(|mut p| {
        p.project_on(take_back_place_ref(place))
            .subslice(from, to, from_end)
    })
}
pub fn ref_place_downcast(place: PlaceRef, variant_index: u32 /*, type */) -> PlaceRef {
    push_place_ref(|mut p| {
        p.project_on(take_back_place_ref(place))
            .downcast(variant_index)
    })
}
pub fn ref_place_opaque_cast(place: PlaceRef /*, type */) -> PlaceRef {
    push_place_ref(|mut p| p.project_on(take_back_place_ref(place)).opaque_cast())
}

pub fn ref_operand_copy(place: PlaceRef) -> OperandRef {
    push_operand_ref(|mut o| o.copy_of(take_back_place_ref(place)))
}
pub fn ref_operand_move(place: PlaceRef) -> OperandRef {
    push_operand_ref(|mut o| o.move_of(take_back_place_ref(place)))
}
pub fn ref_operand_const_bool(value: bool) -> OperandRef {
    push_operand_ref(|mut o| o.const_from().bool(value))
}
pub fn ref_operand_const_int(bit_rep: u128, size: u64, is_signed: bool) -> OperandRef {
    push_operand_ref(|mut o| o.const_from().int(bit_rep, size, is_signed))
}
pub fn ref_operand_const_float(bit_rep: u128, ebits: u64, sbits: u64) -> OperandRef {
    push_operand_ref(|mut o| o.const_from().float(bit_rep, ebits, sbits))
}
pub fn ref_operand_const_char(value: char) -> OperandRef {
    push_operand_ref(|mut o| o.const_from().char(value))
}
pub fn ref_operand_const_func(id: u64) -> OperandRef {
    push_operand_ref(|mut o| o.const_from().func(id))
}
pub fn ref_operand_const_str(value: &str) -> OperandRef {
    push_operand_ref(|mut o| o.const_from().str(value))
}

pub fn assign_use(dest: PlaceRef, operand: OperandRef) {
    assign_to_place_ref(dest).use_of(take_back_operand_ref(operand))
}
pub fn assign_repeat(dest: PlaceRef, operand: OperandRef, count: usize /* constant */) {
    assign_to_place_ref(dest).repeat_of(take_back_operand_ref(operand), count)
}
pub fn assign_ref(dest: PlaceRef, place: PlaceRef, is_mutable: bool) {
    assign_to_place_ref(dest).ref_to(take_back_place_ref(place), is_mutable)
}
pub fn assign_thread_local_ref(
    dest: PlaceRef, /* TODO: Complicated. MIRAI has some works on it. */
) {
    assign_to_place_ref(dest).thread_local_ref_to()
}
pub fn assign_address_of(dest: PlaceRef, place: PlaceRef, is_mutable: bool) {
    assign_to_place_ref(dest).address_of(take_back_place_ref(place), is_mutable)
}
pub fn assign_len(dest: PlaceRef, place: PlaceRef) {
    // To be investigated. Not obvious whether it appears at all in the later stages.
    assign_to_place_ref(dest).len_of(take_back_place_ref(place))
}

pub fn assign_cast_numeric(dest: PlaceRef, operand: OperandRef, is_to_float: bool, size: usize) {
    assign_to_place_ref(dest).numeric_cast_of(take_back_operand_ref(operand), is_to_float, size)
}
pub fn assign_cast(dest: PlaceRef /* TODO: Other types of cast. */) {
    assign_to_place_ref(dest).cast_of()
}

pub fn assign_binary_op(
    dest: PlaceRef,
    operator: BinaryOp,
    first: OperandRef,
    second: OperandRef,
    checked: bool,
) {
    assign_to_place_ref(dest).binary_op_between(
        operator,
        take_back_operand_ref(first),
        take_back_operand_ref(second),
        checked,
    )
}
pub fn assign_unary_op(dest: PlaceRef, operator: UnaryOp, operand: OperandRef) {
    assign_to_place_ref(dest).unary_op_on(operator, take_back_operand_ref(operand))
}

pub fn set_discriminant(dest: PlaceRef, place: PlaceRef, variant_index: u32) {
    todo!()
}
pub fn assign_discriminant(dest: PlaceRef, place: PlaceRef) {
    assign_to_place_ref(dest).discriminant_of(take_back_place_ref(place))
}

// We use slice to simplify working with the interface.
pub fn assign_aggregate_array(dest: PlaceRef, items: &[OperandRef]) {
    assign_to_place_ref(dest).array_from(items.iter().map(|o| take_back_operand_ref(*o)))
}

pub fn take_branch_true(info: BranchingInfo) {
    todo!()
}
pub fn take_branch_false(info: BranchingInfo) {
    todo!()
}

pub fn take_branch_int(info: BranchingInfo, value_bit_rep: u128) {
    todo!()
}
pub fn take_branch_ow_int(info: BranchingInfo, non_values: &[u128]) {
    todo!()
}

pub fn take_branch_char(info: BranchingInfo, value: char) {
    todo!()
}
pub fn take_branch_ow_char(info: BranchingInfo, non_values: &[u128]) {
    todo!()
}

pub fn take_branch_enum_discriminant(info: BranchingInfo, index: VariantIndex) {
    todo!()
}
pub fn take_branch_ow_enum_discriminant(info: BranchingInfo, non_indices: &[VariantIndex]) {
    todo!()
}

pub fn call_func(func: OperandRef, args: &[OperandRef], destination: PlaceRef) {
    todo!()
}
pub fn return_from_func() {
    todo!()
}

pub struct BranchingInfo {
    pub node_location: BasicBlockIndex,
    pub discriminant: OperandRef,
}

impl BranchingInfo {
    pub fn new(node_location: BasicBlockIndex, discriminant: OperandRef) -> Self {
        Self {
            node_location,
            discriminant,
        }
    }
}
