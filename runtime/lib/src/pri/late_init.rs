use std::sync::Once;

use common::pri::*;

use crate::abs;

static INIT: Once = Once::new();

#[derive(Default)]
pub struct LateInitPri<P> {
    _phantom: core::marker::PhantomData<P>,
}

macro_rules! late_init {
    ($(#[$($attr: meta)*])* fn init_runtime_lib ($($(#[$($arg_attr: meta)*])* $arg:ident : $arg_type:ty),* $(,)?) $(-> $ret_ty:ty)?;) => {
        $(#[$($attr)*])*
        #[inline(always)]
        fn init_runtime_lib ($($(#[$($arg_attr)*])* $arg : $arg_type),*) $(-> $ret_ty)? {
            INIT.call_once(|| { });
            super::BasicPri::init_runtime_lib($($arg.into()),*).into()
        }
    };
    ($(#[$($attr: meta)*])* fn $name:ident ($($(#[$($arg_attr: meta)*])* $arg:ident : $arg_type:ty),* $(,)?) $(-> $ret_ty:ty)?;) => {
        $(#[$($attr)*])*
        #[inline(always)]
        fn $name ($($(#[$($arg_attr)*])* $arg : $arg_type),*) $(-> $ret_ty)? {
            if core::intrinsics::likely(INIT.is_completed()) {
                super::BasicPri::$name($($arg.into()),*).into()
            } else {
                super::NoOpPri::$name($($arg.into()),*).into()
            }
        }
    };
}

/* NOTE: Making the implementation generic is desired but not currently possible
 * due to some limitations in Rust like higher-order generic types.
 * Providing an implementation per type is easier at the moment.
 */
impl ProgramRuntimeInterface for LateInitPri<super::BasicPri> {
    type U128 = u128;
    type Char = char;
    type ConstStr = &'static str;
    type ConstByteStr = &'static [u8];
    type Slice<'a, T: 'a> = &'a [T];
    type BranchingInfo = BranchingInfo;
    type TypeId = abs::TypeId;
    type BinaryOp = abs::BinaryOp;
    type UnaryOp = abs::UnaryOp;

    common::pri::list_func_decls! { modifier: late_init, (from Self) }
}
