extern crate rustc_middle;

use rustc_middle::{mir, ty};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Ty(TyKind);

impl<'tcx> From<&ty::Ty<'tcx>> for Ty {
    fn from(ty: &ty::Ty<'tcx>) -> Ty {
        Ty(ty.kind().into())
    }
}

/// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/sty/enum.TyKind.html
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TyKind {
    Bool,
    Char,
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    Adt,     // Adt(AdtDef, SubstsRef),
    Foreign, // Foreign(DefId)
    Str,
    Array, // Array(Ty, Const),
    Slice, // Slice(Ty),
    RawPtr(TypeAndMut),
    Ref(TypeAndMut),  // Ref(Region<'tcx>, Ty<'tcx>, Mutability)
    FnDef,            // FnDef(DefId, SubstsRef<'tcx>)
    FnPtr,            // FnPtr(PolyFnSig<'tcx>)
    Dynamic,          // Dynamic(&'tcx List<Binder<'tcx, ExistentialPredicate<'tcx>>>, Region<'tcx>)
    Closure,          // Closure(DefId, SubstsRef<'tcx>)
    Generator,        // Generator(DefId, SubstsRef<'tcx>, Movability)
    GeneratorWitness, // GeneratorWitness(Binder<'tcx, &'tcx List<Ty<'tcx>>>)
    Never,
    Tuple,       // Tuple(&'tcx List<Ty<'tcx>>)
    Projection,  // Projection(ProjectionTy<'tcx>)
    Opaque,      // Opaque(DefId, SubstsRef<'tcx>)
    Param,       // Param(ParamTy)
    Bound,       // Bound(DebruijnIndex, BoundTy)
    Placeholder, //Placeholder(PlaceholderType)
    Infer,       //Infer(InferTy)
    Error,       //Error(DelaySpanBugEmitted)
}

impl<'tcx> From<&ty::TyKind<'tcx>> for TyKind {
    fn from(t: &ty::TyKind<'tcx>) -> TyKind {
        match t {
            ty::TyKind::Bool => TyKind::Bool,
            ty::TyKind::Char => TyKind::Char,
            ty::TyKind::Int(i) => TyKind::Int(i.into()),
            ty::TyKind::Uint(u) => TyKind::Uint(u.into()),
            ty::TyKind::Float(f) => TyKind::Float(f.into()),
            ty::TyKind::Adt(_, _) => TyKind::Adt,
            ty::TyKind::Foreign(_) => TyKind::Foreign,
            ty::TyKind::Str => TyKind::Str,
            ty::TyKind::Array(_, _) => TyKind::Array,
            ty::TyKind::Slice(_) => TyKind::Slice,
            ty::TyKind::RawPtr(m) => TyKind::RawPtr(m.into()),
            ty::TyKind::Ref(_, t, m) => TyKind::Ref(TypeAndMut {
                ty: Box::new(t.into()),
                mutbl: m.into(),
            }),
            ty::TyKind::FnDef(_, _) => TyKind::FnDef,
            ty::TyKind::FnPtr(_) => TyKind::FnPtr,
            ty::TyKind::Dynamic(_, _) => TyKind::Dynamic,
            ty::TyKind::Closure(_, _) => TyKind::Closure,
            ty::TyKind::Generator(_, _, _) => TyKind::Generator,
            ty::TyKind::GeneratorWitness(_) => TyKind::GeneratorWitness,
            ty::TyKind::Never => TyKind::Never,
            ty::TyKind::Tuple(_) => TyKind::Tuple,
            ty::TyKind::Projection(_) => TyKind::Projection,
            ty::TyKind::Opaque(_, _) => TyKind::Opaque,
            ty::TyKind::Param(_) => TyKind::Param,
            ty::TyKind::Bound(_, _) => TyKind::Bound,
            ty::TyKind::Placeholder(_) => TyKind::Placeholder,
            ty::TyKind::Infer(_) => TyKind::Infer,
            ty::TyKind::Error(_) => TyKind::Error,
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

impl From<&ty::IntTy> for IntTy {
    fn from(t: &ty::IntTy) -> IntTy {
        match t {
            ty::IntTy::Isize => IntTy::Isize,
            ty::IntTy::I8 => IntTy::I8,
            ty::IntTy::I16 => IntTy::I16,
            ty::IntTy::I32 => IntTy::I32,
            ty::IntTy::I64 => IntTy::I64,
            ty::IntTy::I128 => IntTy::I128,
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

impl From<&ty::UintTy> for UintTy {
    fn from(t: &ty::UintTy) -> UintTy {
        match t {
            ty::UintTy::Usize => UintTy::Usize,
            ty::UintTy::U8 => UintTy::U8,
            ty::UintTy::U16 => UintTy::U16,
            ty::UintTy::U32 => UintTy::U32,
            ty::UintTy::U64 => UintTy::U64,
            ty::UintTy::U128 => UintTy::U128,
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum FloatTy {
    F32,
    F64,
}

impl From<&ty::FloatTy> for FloatTy {
    fn from(t: &ty::FloatTy) -> FloatTy {
        match t {
            ty::FloatTy::F32 => FloatTy::F32,
            ty::FloatTy::F64 => FloatTy::F64,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TypeAndMut {
    pub ty: Box<Ty>,
    pub mutbl: Mutability,
}

impl<'tcx> From<&ty::TypeAndMut<'tcx>> for TypeAndMut {
    fn from(t: &ty::TypeAndMut<'tcx>) -> TypeAndMut {
        TypeAndMut {
            ty: Box::new((&t.ty).into()),
            mutbl: (&t.mutbl).into(),
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum Mutability {
    Mut,
    Not,
}

impl From<&mir::Mutability> for Mutability {
    fn from(t: &mir::Mutability) -> Mutability {
        match t {
            mir::Mutability::Mut => Mutability::Mut,
            mir::Mutability::Not => Mutability::Not,
        }
    }
}
