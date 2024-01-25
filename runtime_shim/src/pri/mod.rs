pub mod compiler_helpers;

use common::pri::*;

// Call order: Rust ABI -> ForeignPri (+ conversions) -> runtime lib's C ABI

/*
 * This field serves as a marker to find the module in the compiler easier.
 */
pub static MODULE_MARKER: u8 = 0;

mod ffi {
    use common::ffi::*;

    use super::*;

    macro_rules! declare_fn {
        ($(#[$($attr: meta)*])* fn $name:ident ($($(#[$($arg_attr: meta)*])* $arg:ident : $arg_type:ty),* $(,)?) $(-> $ret_ty:ty)?;) => {
            $(#[$($attr)*])*
            pub(super) fn $name ($($(#[$($arg_attr)*])* $arg : $arg_type),*) $(-> $ret_ty)?;
        };
    }

    #[link(name = "leafrt")]
    extern "C" {
        common::pri::list_func_decls!(modifier: declare_fn, (from common::ffi));
    }

    macro_rules! delegate_to_leafrt {
        ($(#[$($attr: meta)*])* fn $name:ident ($($(#[$($arg_attr: meta)*])* $arg:ident : $arg_type:ty),* $(,)?) $(-> $ret_ty:ty)?;) => {
            $(#[$($attr)*])*
            #[inline(always)]
            fn $name ($($(#[$($arg_attr)*])* $arg : $arg_type),*) $(-> $ret_ty)? {
                unsafe { self::$name($($arg.into()),*).into() }
            }
        };
    }

    pub(super) struct ForeignPri;

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

        list_func_decls!(modifier: delegate_to_leafrt, (from Self));
    }

    impl FfiPri for ForeignPri {}
}

macro_rules! export_to_rust_abi {
    ($(#[$($attr: meta)*])* fn $name:ident ($($(#[$($arg_attr: meta)*])* $arg:ident : $arg_type:ty),* $(,)?) $(-> $ret_ty:ty)?;) => {
        $(#[$($attr)*])*
        pub fn $name ($($(#[$($arg_attr)*])* $arg : $arg_type),*) $(-> $ret_ty)? {
            ffi::ForeignPri::$name($($arg.into()),*)
        }
    };
}

macro_rules! slice_of {
    ($t:ty) => {
        &[$t]
    };
}

common::pri::list_func_decls! {
    modifier: export_to_rust_abi,
    (
        u128: u128,
        char: char,
        &str: &'static str,
        &[u8]: &'static [u8],
        slice: slice_of,
        branching_info: BranchingInfo,
        type_id: TypeId,
        binary_op: BinaryOp,
        unary_op: UnaryOp,
    )
}
