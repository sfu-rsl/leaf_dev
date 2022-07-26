extern crate rustc_middle;

use crate::ty::TyKind;
use crate::{
    consts::{Const, ConstValue},
    place::Place,
    ty::{Mutability, Ty},
};
use rustc_middle::{mir, ty::adjustment};
use serde::{Deserialize, Serialize};
use std::{
    convert::From,
    fmt::{self, Display, Formatter},
    result,
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Rvalue {
    Use(Operand),
    Repeat(Operand, Const),
    Ref(BorrowKind, Place), // Ref(Region, BorrowKind, Place),
    ThreadLocalRef,         // ThreadLocalRef(DefId),
    AddressOf(Mutability, Place),
    Len(Place),
    Cast(CastKind, Operand, Ty),
    BinaryOp(BinOp, Box<(Operand, Operand)>),
    CheckedBinaryOp(BinOp, Box<(Operand, Operand)>),
    NullaryOp(NullOp, Ty),
    UnaryOp(UnOp, Operand),
    Discriminant(Place),
    Aggregate(Box<AggregateKind>, Vec<Operand>),
    ShallowInitBox(Operand, Ty),
}

impl Display for Rvalue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl TryFrom<&str> for Rvalue {
    type Error = serde_json::Error;

    fn try_from(s: &str) -> result::Result<Self, Self::Error> {
        serde_json::from_str(s)
    }
}

impl<'tcx> From<&mir::Rvalue<'tcx>> for Rvalue {
    fn from(t: &mir::Rvalue<'tcx>) -> Rvalue {
        match t {
            mir::Rvalue::Use(o) => Rvalue::Use(o.into()),
            mir::Rvalue::Repeat(o, c) => Rvalue::Repeat(o.into(), c.into()),
            mir::Rvalue::Ref(_, b, p) => Rvalue::Ref(b.into(), p.into()),
            mir::Rvalue::ThreadLocalRef(_) => Rvalue::ThreadLocalRef,
            mir::Rvalue::AddressOf(m, p) => Rvalue::AddressOf(m.into(), p.into()),
            mir::Rvalue::Len(p) => Rvalue::Len(p.into()),
            mir::Rvalue::Cast(c, o, t) => Rvalue::Cast(c.into(), o.into(), t.into()),
            mir::Rvalue::BinaryOp(b, o) => {
                let (o0, o1) = &**o;
                Rvalue::BinaryOp(b.into(), Box::new((o0.into(), o1.into())))
            }
            mir::Rvalue::CheckedBinaryOp(b, o) => {
                let (o0, o1) = &**o;
                Rvalue::CheckedBinaryOp(b.into(), Box::new((o0.into(), o1.into())))
            }
            mir::Rvalue::NullaryOp(n, t) => Rvalue::NullaryOp(n.into(), t.into()),
            mir::Rvalue::UnaryOp(u, o) => Rvalue::UnaryOp(u.into(), o.into()),
            mir::Rvalue::Discriminant(p) => Rvalue::Discriminant(p.into()),
            mir::Rvalue::Aggregate(a, o) => Rvalue::Aggregate(
                Box::new((&**a).into()),
                o.iter().map(|op| op.into()).collect(),
            ),
            mir::Rvalue::ShallowInitBox(o, t) => Rvalue::ShallowInitBox(o.into(), t.into()),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct OperandVec(pub Vec<Operand>);

impl Display for OperandVec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl TryFrom<&str> for OperandVec {
    type Error = serde_json::Error;

    fn try_from(s: &str) -> result::Result<Self, Self::Error> {
        serde_json::from_str(s)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Operand {
    Copy(Place),
    Move(Place),
    Constant(Box<Constant>),
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl<'tcx> From<&mir::Operand<'tcx>> for Operand {
    fn from(t: &mir::Operand<'tcx>) -> Operand {
        match t {
            mir::Operand::Copy(p) => Operand::Copy(p.into()),
            mir::Operand::Move(p) => Operand::Move(p.into()),
            mir::Operand::Constant(_) => Operand::Constant(Box::new(t.constant().unwrap().into())),
        }
    }
}

impl TryFrom<&str> for Operand {
    type Error = serde_json::Error;

    fn try_from(s: &str) -> result::Result<Self, Self::Error> {
        serde_json::from_str(s)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Constant {
    //pub span: Span,
    //pub user_ty: Option<UserTypeAnnotationIndex>,
    pub literal: ConstantKind,
}

impl<'tcx> From<&mir::Constant<'tcx>> for Constant {
    fn from(t: &mir::Constant<'tcx>) -> Constant {
        Constant {
            literal: (&t.literal).into(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum ConstantKind {
    Ty(Const),
    Val(ConstValue, Ty),
}

impl<'tcx> From<&mir::ConstantKind<'tcx>> for ConstantKind {
    fn from(t: &mir::ConstantKind<'tcx>) -> ConstantKind {
        match t {
            mir::ConstantKind::Ty(c) => ConstantKind::Ty(c.into()),
            mir::ConstantKind::Val(c, t) => ConstantKind::Val(c.into(), t.into()),
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum BorrowKind {
    Shared,
    Shallow,
    Unique,
    Mut { allow_two_phase_borrow: bool },
}

impl From<&mir::BorrowKind> for BorrowKind {
    fn from(t: &mir::BorrowKind) -> BorrowKind {
        match *t {
            mir::BorrowKind::Shared => BorrowKind::Shared,
            mir::BorrowKind::Shallow => BorrowKind::Shallow,
            mir::BorrowKind::Unique => BorrowKind::Unique,
            mir::BorrowKind::Mut {
                allow_two_phase_borrow,
            } => BorrowKind::Mut {
                allow_two_phase_borrow,
            },
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum CastKind {
    Misc,
    Pointer(PointerCast),
}

impl From<&mir::CastKind> for CastKind {
    fn from(t: &mir::CastKind) -> CastKind {
        match t {
            mir::CastKind::Misc => CastKind::Misc,
            mir::CastKind::Pointer(p) => CastKind::Pointer(p.into()),
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum PointerCast {
    ReifyFnPointer,
    UnsafeFnPointer,
    ClosureFnPointer, // ClosureFnPointer(Unsafety),
    MutToConstPointer,
    ArrayToPointer,
    Unsize,
}

impl From<&adjustment::PointerCast> for PointerCast {
    fn from(t: &adjustment::PointerCast) -> PointerCast {
        match t {
            adjustment::PointerCast::ReifyFnPointer => PointerCast::ReifyFnPointer,
            adjustment::PointerCast::UnsafeFnPointer => PointerCast::UnsafeFnPointer,
            adjustment::PointerCast::ClosureFnPointer(_) => PointerCast::ClosureFnPointer,
            adjustment::PointerCast::MutToConstPointer => PointerCast::MutToConstPointer,
            adjustment::PointerCast::ArrayToPointer => PointerCast::ArrayToPointer,
            adjustment::PointerCast::Unsize => PointerCast::Unsize,
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    Offset,
}

impl From<&mir::BinOp> for BinOp {
    fn from(t: &mir::BinOp) -> BinOp {
        match t {
            mir::BinOp::Add => BinOp::Add,
            mir::BinOp::Sub => BinOp::Sub,
            mir::BinOp::Mul => BinOp::Mul,
            mir::BinOp::Div => BinOp::Div,
            mir::BinOp::Rem => BinOp::Rem,
            mir::BinOp::BitXor => BinOp::BitXor,
            mir::BinOp::BitAnd => BinOp::BitAnd,
            mir::BinOp::BitOr => BinOp::BitOr,
            mir::BinOp::Shl => BinOp::Shl,
            mir::BinOp::Shr => BinOp::Shr,
            mir::BinOp::Eq => BinOp::Eq,
            mir::BinOp::Lt => BinOp::Lt,
            mir::BinOp::Le => BinOp::Le,
            mir::BinOp::Ne => BinOp::Ne,
            mir::BinOp::Ge => BinOp::Ge,
            mir::BinOp::Gt => BinOp::Gt,
            mir::BinOp::Offset => BinOp::Offset,
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum NullOp {
    SizeOf,
    AlignOf,
}

impl From<&mir::NullOp> for NullOp {
    fn from(t: &mir::NullOp) -> NullOp {
        match t {
            mir::NullOp::SizeOf => NullOp::SizeOf,
            mir::NullOp::AlignOf => NullOp::AlignOf,
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum UnOp {
    Not,
    Neg,
}

impl From<&mir::UnOp> for UnOp {
    fn from(t: &mir::UnOp) -> UnOp {
        match t {
            mir::UnOp::Not => UnOp::Not,
            mir::UnOp::Neg => UnOp::Neg,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum AggregateKind {
    Array(Ty),
    // No need to handle other ones and possibly arrays in the future.
    // See https://github.com/rust-lang/rust/issues/48193.
    //Tuple,
    //Adt(VariantIdx, Option<usize>), // Adt(DefId, VariantIdx, SubstsRef<'tcx>, Option<UserTypeAnnotationIndex>, Option<usize>),
    //Closure,   // Closure(DefId, SubstsRef<'tcx>),
    //Generator, // Generator(DefId, SubstsRef<'tcx>, Movability),
    Unsupported,
}

impl<'tcx> From<&mir::AggregateKind<'tcx>> for AggregateKind {
    fn from(t: &mir::AggregateKind<'tcx>) -> AggregateKind {
        match t {
            mir::AggregateKind::Array(t) => AggregateKind::Array(t.into()),
            _ => AggregateKind::Unsupported,
        }
    }
}
