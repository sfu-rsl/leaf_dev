use std::{cell::RefCell, sync::Mutex};

#[macro_use]
extern crate lazy_static;

mod runtime;

lazy_static! {
    static ref RUNTIME: Mutex<RuntimeImpl> = todo!();
}

struct RuntimeImpl {}

impl runtime::Runtime for RuntimeImpl {
    fn assign<T: runtime::Assignment>() -> T {
        todo!()
    }

    fn branch<T: runtime::Branching>() -> T {
        todo!()
    }

    fn function<T: runtime::Function>() -> T {
        todo!()
    }
}

pub type Local = u32;
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

pub type BasicBlockIndex = u32;
pub type BranchTarget = BasicBlockIndex;

pub fn ref_place_local(local: Local) -> PlaceRef {
    todo!()
}
pub fn ref_place_deref(place: PlaceRef) -> PlaceRef {
    todo!()
}
pub fn ref_place_field(place: PlaceRef, field: u32 /*, type */) -> PlaceRef {
    todo!()
}
pub fn ref_place_index(place: PlaceRef, index_place: PlaceRef) -> PlaceRef {
    todo!()
}
pub fn ref_place_constant_index(
    place: PlaceRef,
    offset: u64,
    min_length: u64,
    from_end: bool,
) -> PlaceRef {
    todo!()
}
pub fn ref_place_subslice(place: PlaceRef, from: u64, to: u64, from_end: bool) -> PlaceRef {
    todo!()
}
pub fn ref_place_downcast(place: PlaceRef, variant_index: u32 /*, type */) -> PlaceRef {
    todo!()
}
pub fn ref_place_opaque_cast(place: PlaceRef /*, type */) -> PlaceRef {
    todo!()
}

pub fn ref_operand_copy(place: PlaceRef) -> OperandRef {
    todo!()
}
pub fn ref_operand_move(place: PlaceRef) -> OperandRef {
    todo!()
}
pub fn ref_operand_const_bool(value: bool) -> OperandRef {
    todo!()
}
pub fn ref_operand_const_int(bit_rep: u128, size: u64, is_signed: bool) -> OperandRef {
    todo!()
}
pub fn ref_operand_const_float(bit_rep: u128, ebits: u64, sbits: u64) -> OperandRef {
    todo!()
}
pub fn ref_operand_const_char(value: char) -> OperandRef {
    todo!()
}

pub fn assign_use(dest: PlaceRef, operand: OperandRef) {
    todo!()
}
pub fn assign_repeat(dest: PlaceRef, operand: OperandRef, count: usize /* constant */) {
    todo!()
}
pub fn assign_ref(dest: PlaceRef, place: PlaceRef, is_mutable: bool) {
    todo!()
}
pub fn assign_thread_local_ref(
    dest: PlaceRef, /* TODO: Complicated. MIRAI has some works on it. */
) {
    todo!()
}
pub fn assign_address_of(dest: PlaceRef, place: PlaceRef, is_mutable: bool) {
    todo!()
}
pub fn assign_len(dest: PlaceRef, place: PlaceRef) {
    // To be investigated. Not obvious whether it appears at all in the later stages.
    todo!()
}

pub fn assign_cast_numeric(dest: PlaceRef, operand: OperandRef, is_to_float: bool, size: usize) {
    todo!()
}
pub fn assign_cast(dest: PlaceRef /* TODO: Other types of cast. */) {
    todo!()
}

pub fn assign_binary_op(
    dest: PlaceRef,
    operator: BinaryOp,
    first: OperandRef,
    second: OperandRef,
    checked: bool,
) {
    todo!()
}
pub fn assign_unary_op(dest: PlaceRef, operator: UnaryOp, operand: OperandRef) {
    todo!()
}

pub fn set_discriminant(dest: PlaceRef, place: PlaceRef, variant_index: u32) {
    todo!()
}
pub fn assign_discriminant(dest: PlaceRef, place: PlaceRef) {
    todo!()
}

// We use slice to simplify working with the interface.
pub fn assign_aggregate_array(dest: PlaceRef, items: &[OperandRef]) {
    todo!()
}

pub fn switch_int(
    discriminant: OperandRef,
    branches: &[(u128, BranchTarget)],
    otherwise_target: BranchTarget,
) {
    todo!()
}

pub fn fn_call(func: OperandRef, args: &[OperandRef], destination: PlaceRef) {
    todo!()
}
pub fn fn_return() {
    todo!()
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    Offset,
}

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}
