use crate::{abs, pri::*};
use common::ffi::*;

macro_rules! extern_fn {
    ($(#[$($attr: meta)*])* fn $name:ident ($($(#[$($arg_attr: meta)*])* $arg:ident : $arg_type:ty),* $(,)?) $(-> $ret_ty:ty)?;) => {
        $(#[$($attr)*])*
        #[no_mangle]
        pub extern "C" fn $name ($($(#[$($arg_attr)*])* $arg : $arg_type),*) $(-> $ret_ty)? {
            crate::pri::$name($($arg.into()),*)
        }
    };
}

macro_rules! slice_of { ($t:ty) => { SlicePack<$t> }; }

common::pri::list_func_decls! {
    modifier: extern_fn,
    (u128: U128Pack, char: CharPack, &str: ConstStrPack, [u8]: ConstByteStrPack, slice: slice_of, branching_info: BranchingInfo, type_id: U128Pack<TypeId>)
}

impl From<common::pri::BinaryOp> for abs::BinaryOp {
    fn from(value: common::pri::BinaryOp) -> Self {
        unsafe { core::mem::transmute(value) }
    }
}

impl From<common::pri::UnaryOp> for abs::UnaryOp {
    fn from(value: common::pri::UnaryOp) -> Self {
        unsafe { core::mem::transmute(value) }
    }
}
