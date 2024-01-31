use common::{ffi::*, pri::*};

use crate::abs;

// Call order: C ABI -> ForeignPri (+ conversions) -> DefaultPri

struct ForeignPri;

macro_rules! delegate_to_default {
    ($(#[$($attr: meta)*])* fn $name:ident ($($(#[$($arg_attr: meta)*])* $arg:ident : $arg_type:ty),* $(,)?) $(-> $ret_ty:ty)?;) => {
        $(#[$($attr)*])*
        #[inline(always)]
        fn $name ($($(#[$($arg_attr)*])* $arg : $arg_type),*) $(-> $ret_ty)? {
            crate::pri::PriImpl::$name($($arg.into()),*).into()
        }
    };
}

/* NOTE: Why do we add a layer of indirection and implement this trait?
 * Refer to the comment in the common library.
 * Basically, making sure that no function is missed in the exported interface.
 */

impl ProgramRuntimeInterface for ForeignPri {
    type U128 = U128Pack;
    type Char = CharPack;
    type ConstStr = ConstStrPack;
    type ConstByteStr = ConstByteStrPack;
    type Slice<'a, T: 'a> = SlicePack<T>;
    type BranchingInfo = common::pri::BranchingInfo;
    type TypeId = U128Pack<TypeId>;
    type BinaryOp = common::pri::BinaryOp;
    type UnaryOp = common::pri::UnaryOp;

    common::pri::list_func_decls!(modifier: delegate_to_default, (from Self));
}
impl FfiPri for ForeignPri {}

impl From<common::pri::BinaryOp> for abs::BinaryOp {
    #[inline(always)]
    fn from(value: common::pri::BinaryOp) -> Self {
        unsafe { core::mem::transmute(value) }
    }
}

impl From<common::pri::UnaryOp> for abs::UnaryOp {
    #[inline(always)]
    fn from(value: common::pri::UnaryOp) -> Self {
        unsafe { core::mem::transmute(value) }
    }
}

impl From<common::pri::BranchingInfo> for crate::pri::DefaultBranchingInfo {
    #[inline(always)]
    fn from(value: common::pri::BranchingInfo) -> Self {
        Self::new(
            value.node_location,
            value.discriminant,
            value.discr_bit_size,
            value.discr_is_signed,
        )
    }
}
impl From<crate::pri::DefaultBranchingInfo> for common::pri::BranchingInfo {
    #[inline(always)]
    fn from(value: crate::pri::DefaultBranchingInfo) -> Self {
        Self {
            node_location: value.metadata.node_location,
            discriminant: value.discriminant,
            discr_bit_size: value.metadata.discr_as_int.bit_size,
            discr_is_signed: value.metadata.discr_as_int.is_signed,
        }
    }
}

macro_rules! export_to_c_abi {
    ($(#[$($attr: meta)*])* fn $name:ident ($($(#[$($arg_attr: meta)*])* $arg:ident : $arg_type:ty),* $(,)?) $(-> $ret_ty:ty)?;) => {
        $(#[$($attr)*])*
        #[no_mangle]
        pub extern "C" fn $name ($($(#[$($arg_attr)*])* $arg : $arg_type),*) $(-> $ret_ty)? {
            ForeignPri::$name($($arg),*)
        }
    };
}

common::pri::list_func_decls!(modifier: export_to_c_abi, (from common::ffi));
