pub mod compiler_helpers;

use common::{pri::*, *};

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

}

macro_rules! intern_fn {
    ($(#[$($attr: meta)*])* fn $name:ident ($($(#[$($arg_attr: meta)*])* $arg:ident : $arg_type:ty),* $(,)?) $(-> $ret_ty:ty)?;) => {
        $(#[$($attr)*])*
        pub fn $name ($($(#[$($arg_attr)*])* $arg : $arg_type),*) $(-> $ret_ty)? {
            unsafe { ffi::$name($($arg.into()),*) }
        }
    };
}

macro_rules! slice_of {
    ($t:ty) => {
        &[$t]
    };
}

common::pri::list_func_decls! {
    modifier: intern_fn,
    (u128: u128, char: char, &str: &'static str, [u8]: &'static [u8], slice: slice_of, branching_info: BranchingInfo, type_id: TypeId)
}
