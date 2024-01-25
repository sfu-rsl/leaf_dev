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

common::pri::list_func_decls!(modifier: export_to_c_abi, (from common::ffi));

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
