use common::pri::*;

use crate::abs::{self, BranchingMetadata, IntType};

pub struct NoOpPri;

macro_rules! noop {
    ($(#[$($attr: meta)*])* fn $name:ident ($($(#[$($arg_attr: meta)*])* $arg:ident : $arg_type:ty),* $(,)?) $(-> $ret_ty:ty)?;) => {
        $(#[$($attr)*])*
        #[inline(always)]
        fn $name ($($(#[$($arg_attr)*])* $arg : $arg_type),*) $(-> $ret_ty)? {
            Default::default()
        }
    };
}

use super::DefaultBranchingInfo as BranchingInfo;

impl ProgramRuntimeInterface for NoOpPri {
    // We try to match the DefaultPri, but it is not necessary to do so if it causes significant overhead.
    type U128 = u128;
    type Char = char;
    type ConstStr = &'static str;
    type ConstByteStr = &'static [u8];
    type Slice<'a, T: 'a> = &'a [T];
    type BranchingInfo = BranchingInfo;
    type TypeId = abs::TypeId;
    type BinaryOp = abs::BinaryOp;
    type UnaryOp = abs::UnaryOp;

    common::pri::list_func_decls! { modifier: noop, (from Self) }
}

impl Default for BranchingInfo {
    fn default() -> Self {
        Self {
            discriminant: Default::default(),
            metadata: BranchingMetadata {
                discr_as_int: IntType {
                    bit_size: 0,
                    is_signed: false,
                },
                node_location: 0,
            },
        }
    }
}
