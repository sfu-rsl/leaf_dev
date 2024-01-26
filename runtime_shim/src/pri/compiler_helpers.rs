// use super::*;

use common::{pri::*, *};

pub static CH_MODULE_MARKER: u8 = 0;

/* These fields serve as exported symbols to get the types of the desired
 * arguments easier in the compiler. */
pub static PLACE_REF_TYPE_HOLDER: PlaceRef = 0;
pub static OPERAND_REF_TYPE_HOLDER: OperandRef = 0;
pub static BINARY_OP_TYPE_HOLDER: BinaryOp = BinaryOp::ADD;
pub static UNARY_OP_TYPE_HOLDER: UnaryOp = UnaryOp::NEG;
pub static RAW_PTR_TYPE_HOLDER: RawPointer = 0;
pub static FUNC_ID_TYPE_HOLDER: FuncId = 0;

pub const fn f32_to_bits(value: f32) -> u128 {
    value.to_bits() as u128
}

pub const fn f64_to_bits(value: f64) -> u128 {
    value.to_bits() as u128
}

pub const fn mark_as_nctfe() {}

/* NOTE:
 * This is a workaround to prevent the compiler from removing the generic
 * functions from the exported symbols as they are unused.
 */

#[used]
#[cfg(place_addr)]
static _SET_PLACE_ADDR_TYPED_REFERENCER: fn(PlaceRef, *const u32) -> () =
    set_place_address_typed::<u32>;

#[used]
static _TYPE_ID_OF_REFERENCER: fn() -> TypeId = type_id_of::<u32>;

#[used]
static _SIZE_OF_REFERENCER: fn() -> TypeSize = size_of::<u32>;

#[used]
static _CONST_BINARY_OP_OF_REFERENCER: fn(u8) -> BinaryOp = const_binary_op_of;

#[used]
static _CONST_UNARY_OP_OF_REFERENCER: fn(u8) -> UnaryOp = const_unary_op_of;

pub fn set_place_address_typed<T>(place: PlaceRef, address: *const T) {
    super::set_place_address(place, address.cast::<()>() as RawPointer)
}

pub fn type_id_of<T: ?Sized + 'static>() -> TypeId {
    /* NOTE: Once this function is const in stable build, we can mark this
     * function as constant as well. */
    /* NOTE: Do we need to bother about inlining?
     * Based on the last checks, LLVM is smart enough to inline this function
     * automatically and even replace everything with u128.
     * Also, giving this function the `inline` attribute will cause it to
     * not be exported. */
    common::utils::type_id_of::<T>()
}

pub fn size_of<T>() -> TypeSize {
    core::mem::size_of::<T>() as TypeSize
}

pub const fn const_binary_op_of(raw: u8) -> BinaryOp {
    BinaryOp::from_raw(raw)
}

pub const fn const_unary_op_of(raw: u8) -> UnaryOp {
    UnaryOp::from_raw(raw)
}
