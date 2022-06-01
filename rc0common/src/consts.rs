//! TODO: Make sure that getting the actual data of a const can be done via reading the local.

extern crate rustc_middle;
extern crate rustc_target;

use crate::ty::Ty;
use rustc_middle::{mir::interpret, ty};
use rustc_target::abi;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Const {
    pub ty: Ty,
    pub val: ConstKind,
}

impl<'tcx> From<&ty::Const<'tcx>> for Const {
    fn from(t: &ty::Const<'tcx>) -> Const {
        Const {
            ty: (&t.ty()).into(),
            val: (&t.val()).into(),
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum ConstKind {
    Param(ParamConst),
    Infer,       // Infer(InferConst)
    Bound,       // Bound(DebruijnIndex, BoundVar)
    Placeholder, //Placeholder(PlaceholderConst)
    Unevaluated, // Unevaluated(Unevaluated),
    Value(ConstValue),
    Error, // Error(DelaySpanBugEmitted)
}

impl<'tcx> From<&ty::ConstKind<'tcx>> for ConstKind {
    fn from(t: &ty::ConstKind<'tcx>) -> ConstKind {
        match t {
            ty::ConstKind::Param(p) => ConstKind::Param(p.into()),
            ty::ConstKind::Infer(_) => ConstKind::Infer,
            ty::ConstKind::Bound(_, _) => ConstKind::Bound,
            ty::ConstKind::Placeholder(_) => ConstKind::Placeholder,
            ty::ConstKind::Unevaluated(_) => ConstKind::Unevaluated,
            ty::ConstKind::Value(c) => ConstKind::Value(c.into()),
            ty::ConstKind::Error(_) => ConstKind::Error,
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct ParamConst {
    pub index: u32,
    //pub name: Symbol,
}

impl From<&ty::ParamConst> for ParamConst {
    fn from(t: &ty::ParamConst) -> ParamConst {
        ParamConst { index: t.index }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum ConstValue {
    Scalar(Scalar),
    Slice {
        //data: ConstAllocation,
        start: usize,
        end: usize,
    },
    ByRef {
        //alloc: ConstAllocation,
        offset: Size,
    },
}

impl<'tcx> From<&interpret::ConstValue<'tcx>> for ConstValue {
    fn from(t: &interpret::ConstValue<'tcx>) -> ConstValue {
        match t {
            interpret::ConstValue::Scalar(s) => ConstValue::Scalar(s.into()),
            interpret::ConstValue::Slice {
                data: _,
                start,
                end,
            } => ConstValue::Slice {
                start: *start,
                end: *end,
            },
            interpret::ConstValue::ByRef { alloc: _, offset } => ConstValue::ByRef {
                offset: offset.into(),
            },
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum Scalar {
    Int, // Int(ScalarInt),
    Ptr, // Ptr(Pointer, u8),
}

impl From<&interpret::Scalar> for Scalar {
    fn from(t: &interpret::Scalar) -> Scalar {
        match t {
            interpret::Scalar::Int(_) => Scalar::Int,
            interpret::Scalar::Ptr(_, _) => Scalar::Ptr,
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct Size {
    raw: u64,
}

impl From<&abi::Size> for Size {
    fn from(t: &abi::Size) -> Size {
        Size { raw: t.bytes() }
    }
}
