use super::common;
use common::pri::*;

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub static CH_MODULE_MARKER: u8 = 0;

/* These fields serve as exported symbols to get the types of the desired
 * arguments easier in the compiler. */
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub static PLACE_REF_TYPE_HOLDER: PlaceRef = 0;
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub static OPERAND_REF_TYPE_HOLDER: OperandRef = 0;
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub static BINARY_OP_TYPE_HOLDER: BinaryOp = BinaryOp::ADD;
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub static UNARY_OP_TYPE_HOLDER: UnaryOp = UnaryOp::NEG;

/* NOTE: The const version of this conversion is unstable
 * and causes errors during the compilation of the core library.
 */
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub fn f32_to_bits(value: f32) -> u128 {
    unsafe { core::intrinsics::transmute::<f32, u32>(value) as u128 }
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub fn f64_to_bits(value: f64) -> u128 {
    unsafe { core::intrinsics::transmute::<f64, u64>(value) as u128 }
}

/* NOTE:
 * This is a workaround to prevent the compiler from removing the generic
 * functions from the exported symbols as they are unused.
 */

#[used]
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

#[used]
static _CONST_ATOMIC_ORD_OF_REFERENCER: fn(u8) -> AtomicOrdering = const_atomic_ord_of;

#[used]
static _CONST_ATOMIC_BINARY_OP_OF_REFERENCER: fn(u8) -> AtomicBinaryOp = const_atomic_binary_op_of;

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub fn set_place_address_typed<T>(place: PlaceRef, address: *const T) {
    super::set_place_address(place, address as *const () as RawPointer)
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[cfg_attr(
    core_build,
    rustc_const_unstable(feature = "const_eval_select", issue = "124625")
)]
#[inline(always)]
pub const fn type_id_of<T: ?Sized + 'static>() -> TypeId {
    /* NOTE: Once this function is const in stable build, we can mark this
     * function as constant as well. */
    /* NOTE: Do we need to bother about inlining?
     * Based on the last checks, LLVM is smart enough to inline this function
     * automatically and even replace everything with u128. */
    fn rt<T: ?Sized + 'static>() -> TypeId {
        super::run_rec_guarded(
            /* If we are recursing, the value doesn't matter (although unsafe) */
            unsafe { core::intrinsics::transmute([0u8; core::intrinsics::size_of::<TypeId>()]) },
            || common::utils::type_id_of::<T>(),
        )
    }
    core::intrinsics::const_eval_select((), common::utils::type_id_of::<T>, rt::<T>)
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[cfg_attr(core_build, rustc_const_stable(feature = "rust1", since = "1.0.0"))]
#[inline(always)]
pub const fn size_of<T>() -> TypeSize {
    core::intrinsics::size_of::<T>() as TypeSize
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[cfg_attr(core_build, rustc_const_stable(feature = "rust1", since = "1.0.0"))]
#[inline(always)]
pub const fn const_binary_op_of(raw: u8) -> BinaryOp {
    BinaryOp::from_raw(raw)
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[cfg_attr(core_build, rustc_const_stable(feature = "rust1", since = "1.0.0"))]
#[inline(always)]
pub const fn const_unary_op_of(raw: u8) -> UnaryOp {
    UnaryOp::from_raw(raw)
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[cfg_attr(core_build, rustc_const_stable(feature = "rust1", since = "1.0.0"))]
#[inline(always)]
pub const fn const_atomic_ord_of(raw: u8) -> AtomicOrdering {
    AtomicOrdering::from_raw(raw)
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[cfg_attr(core_build, rustc_const_stable(feature = "rust1", since = "1.0.0"))]
#[inline(always)]
pub const fn const_atomic_binary_op_of(raw: u8) -> AtomicBinaryOp {
    AtomicBinaryOp::from_raw(raw)
}

/* This function is used as a replacement for special functions like intrinsics
 * that cannot be treated as ordinary ones. */
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub fn special_func_placeholder() {}
