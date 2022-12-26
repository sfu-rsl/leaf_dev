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

type Ref = u64;
type PlaceRef = Ref;
type OperandRef = Ref;

type BasicBlockIndex = u32;
type BranchTarget = BasicBlockIndex;

fn ref_place_local(local: u32) -> PlaceRef {
    todo!()
}
fn ref_place_deref(place: PlaceRef) -> PlaceRef {
    todo!()
}
fn ref_place_field(place: PlaceRef, field: u32 /*, type */) -> PlaceRef {
    todo!()
}
fn ref_place_index(place: PlaceRef, index_place: PlaceRef) -> PlaceRef {
    todo!()
}
fn ref_place_constant_index(
    place: PlaceRef,
    offset: u64,
    min_length: u64,
    from_end: bool,
) -> PlaceRef {
    todo!()
}
fn ref_place_sublice(place: PlaceRef, from: u64, to: u64, from_end: bool) -> PlaceRef {
    todo!()
}
fn ref_place_downcast(place: PlaceRef, variant_index: u32 /*, type */) -> PlaceRef {
    todo!()
}
fn ref_place_opaque_cast(place: PlaceRef /*, type */) -> PlaceRef {
    todo!()
}

fn ref_operand_copy(place: PlaceRef) -> OperandRef {
    todo!()
}
fn ref_operand_move(place: PlaceRef) -> OperandRef {
    todo!()
}
fn ref_operand_const(/* TODO */) -> OperandRef {
    todo!()
}

fn assign_use(place: PlaceRef, operand: OperandRef) {
    todo!()
}
fn assign_repeat(operand: OperandRef, count: u32 /* constant */) {
    todo!()
}
fn assign_ref(place: PlaceRef, is_mutable: bool) {
    todo!()
}
fn assign_thread_local_ref(/* TODO: Complicated. MIRAI has some works on it. */) {
    todo!()
}
fn assign_address_of(place: PlaceRef, is_mutable: bool) {
    todo!()
}
fn assign_len(place: PlaceRef) {
    // To be investigated. Not obvious whether it appears at all in the later stages.
    todo!()
}

fn assign_cast_numeric(operand: OperandRef, is_to_float: bool, size: usize) {
    todo!()
}
fn assign_cast(/* TODO: Other types of cast. */) {
    todo!()
}

fn assign_binary_op(operator: BinaryOp, first: OperandRef, second: OperandRef, checked: bool) {
    todo!()
}
fn assign_unary_op(operator: UnaryOp, operand: OperandRef) {
    todo!()
}

fn set_discriminant(place: PlaceRef, variant_index: u32) {
    todo!()
}
fn assign_discriminant(place: PlaceRef) {
    todo!()
}

// We use slice to simplify working with the interface.
fn assign_aggregate_array(items: &[OperandRef]) {
    todo!()
}

fn switch_int(
    discriminant: OperandRef,
    branches: &[(u128, BranchTarget)],
    otherwise_target: BranchTarget,
) {
    todo!()
}

fn fn_call(func: OperandRef, args: &[OperandRef], destination: PlaceRef) {
    todo!()
}
fn fn_return() {
    todo!()
}

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

pub enum UnaryOp {
    Not,
    Neg,
}
