mod instance;
mod utils;

use crate::abs::{
    AssignmentHandler, BasicBlockIndex, BinaryOp, BranchTakingHandler, BranchingHandler,
    ConstantHandler, FunctionHandler, Local, OperandHandler, PlaceHandler, PlaceProjectionHandler,
    UnaryOp, VariantIndex, FieldIndex,
};

use self::instance::*;

pub type Ref = u64;
pub type PlaceRef = Ref;
pub type OperandRef = Ref;

/*
 * These fields serve as exported symbols in the workaround to get the Tys for
 * the desired types in the compiler.
 */
pub static PLACE_REF_TYPE_HOLDER: PlaceRef = 0;
pub static OPERAND_REF_TYPE_HOLDER: OperandRef = 0;
pub static BINARY_OP_TYPE_HOLDER: BinaryOp = BinaryOp::Add;
pub static UNARY_OP_TYPE_HOLDER: UnaryOp = UnaryOp::Neg;

pub fn init_runtime_lib() {
    init_backend()
}

pub fn ref_place_local(local: Local) -> PlaceRef {
    push_place_ref(|p| p.of_local(local))
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

pub fn assign_use(dest: PlaceRef, operand: OperandRef) {
    assign_to(dest, |h| h.use_of(take_back_operand_ref(operand)))
}
pub fn assign_repeat(dest: PlaceRef, operand: OperandRef, count: usize /* constant */) {
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

pub fn assign_cast_numeric(dest: PlaceRef, operand: OperandRef, is_to_float: bool, size: usize) {
    assign_to(dest, |h| {
        h.numeric_cast_of(take_back_operand_ref(operand), is_to_float, size)
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

pub fn set_discriminant(dest: PlaceRef, place: PlaceRef, variant_index: u32) {
    todo!()
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
    branch(info, |h| h.on_bool().take(true))
}
pub fn take_branch_false(info: BranchingInfo) {
    branch(info, |h| h.on_bool().take(false))
}

pub fn take_branch_int(info: BranchingInfo, value_bit_rep: u128) {
    branch(info, |h| h.on_int().take(value_bit_rep))
}
pub fn take_branch_ow_int(info: BranchingInfo, non_values: &[u128]) {
    branch(info, |h| h.on_int().take_otherwise(non_values))
}

pub fn take_branch_char(info: BranchingInfo, value: char) {
    branch(info, |h| h.on_char().take(value))
}
pub fn take_branch_ow_char(info: BranchingInfo, non_values: &[u128]) {
    branch(info, |h| h.on_char().take_otherwise(non_values))
}

pub fn take_branch_enum_discriminant(info: BranchingInfo, index: VariantIndex) {
    branch(info, |h| h.on_enum().take(index))
}
pub fn take_branch_ow_enum_discriminant(info: BranchingInfo, non_indices: &[VariantIndex]) {
    branch(info, |h| h.on_enum().take_otherwise(non_indices))
}

pub fn call_func(func: OperandRef, args: &[OperandRef], destination: PlaceRef) {
    func_control(|h| {
        h.call(
            take_back_operand_ref(func),
            args.iter().map(|o| take_back_operand_ref(*o)),
            take_back_place_ref(destination),
        )
    })
}
pub fn return_from_func() {
    func_control(|h| h.ret())
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
