use std::{cell::RefCell, sync::Mutex};

use runtime::{PlaceHandler, PlaceProjectionHandler, Runtime};

#[macro_use]
extern crate lazy_static;

mod runtime;

lazy_static! {
    static ref RUNTIME: Mutex<RuntimeImpl> = todo!();
}

type RuntimeImpl = runtime::fake::FakeRuntime;
type PlaceImpl = <<RuntimeImpl as runtime::Runtime>::PlaceHandler as runtime::PlaceHandler>::Place;

pub type Local = u32;

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
pub fn ref_operand_const_func(id: u64) -> OperandRef {
    todo!()
}
pub fn ref_operand_const_str(value: &str) -> OperandRef {
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

fn push_place_ref(
    get_place: impl FnOnce(<RuntimeImpl as runtime::Runtime>::PlaceHandler) -> PlaceImpl,
) -> PlaceRef {
    get_place_ref_manager().push(get_place(get_runtime().place()))
}

fn take_back_place_ref(reference: PlaceRef) -> PlaceImpl {
    get_place_ref_manager().take_back(reference)
}

fn get_runtime() -> &'static mut RuntimeImpl {
    todo!()
}

fn get_place_ref_manager() -> &'static mut DefaultRefManager<PlaceImpl> {
    todo!()
}

trait RefManager {
    type Ref;
    type Value;

    fn push(&mut self, value: Self::Value) -> Self::Ref;

    fn take_back(&mut self, reference: Self::Ref) -> Self::Value;
}

struct DefaultRefManager<V> {
    counter: Ref,
    refs: Vec<(Ref, V)>,
}

impl<V> RefManager for DefaultRefManager<V> {
    type Ref = Ref;
    type Value = V;

    fn push(&mut self, value: V) -> Ref {
        self.counter += 1;
        self.refs.push((self.counter, value));
        self.counter
    }

    fn take_back(&mut self, reference: Ref) -> V {
        let index = self
            .refs
            .iter()
            .position(|(r, _)| r.eq(&reference))
            .unwrap();
        self.refs.swap_remove(index).1
    }
}
