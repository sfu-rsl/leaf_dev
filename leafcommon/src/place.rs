extern crate rustc_middle;
extern crate rustc_span;
extern crate rustc_target;

use crate::ty::Ty;
use rustc_middle::mir;
use rustc_target::abi;
use serde::{Deserialize, Serialize};
use std::hash::Hash;
use std::{
    convert::From,
    fmt::{self, Display, Formatter},
    result,
};

/// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.Place.html
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Place {
    pub local: Local,
    pub projection: Vec<PlaceElem>,
}

impl Display for Place {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl<'tcx> From<&mir::Place<'tcx>> for Place {
    fn from(pl: &mir::Place) -> Place {
        Place {
            local: (&pl.local).into(),
            projection: pl.projection.iter().map(|pe| (&pe).into()).collect(),
        }
    }
}

impl TryFrom<&str> for Place {
    type Error = serde_json::Error;

    fn try_from(s: &str) -> result::Result<Self, Self::Error> {
        serde_json::from_str(s)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum PlaceElem {
    Deref,
    Field(Field, Ty),
    Index(Local),
    ConstantIndex {
        offset: u64,
        min_length: u64,
        from_end: bool,
    },
    Subslice {
        from: u64,
        to: u64,
        from_end: bool,
    },
    Downcast(VariantIdx), // Downcast(Option<Symbol>, VariantIdx),
}

impl<'tcx> From<&mir::PlaceElem<'tcx>> for PlaceElem {
    fn from(p: &mir::PlaceElem) -> PlaceElem {
        match *p {
            mir::PlaceElem::Deref => PlaceElem::Deref,
            mir::PlaceElem::Field(f, t) => PlaceElem::Field((&f).into(), (&t).into()),
            mir::PlaceElem::Index(l) => PlaceElem::Index((&l).into()),
            mir::PlaceElem::ConstantIndex {
                offset,
                min_length,
                from_end,
            } => PlaceElem::ConstantIndex {
                offset,
                min_length,
                from_end,
            },
            mir::PlaceElem::Subslice { from, to, from_end } => {
                PlaceElem::Subslice { from, to, from_end }
            }
            mir::PlaceElem::Downcast(_, v) => PlaceElem::Downcast((&v).into()),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Local {
    private: u32,
}

impl Local {
    pub fn get_private(&self) -> u32 {
        self.private
    }
}

impl From<&mir::Local> for Local {
    fn from(l: &mir::Local) -> Local {
        Local {
            private: l.as_u32(),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Field {
    private: u32,
}

impl From<&mir::Field> for Field {
    fn from(f: &mir::Field) -> Field {
        Field {
            private: f.as_u32(),
        }
    }
}

//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//pub struct Symbol(SymbolIndex);
//
//impl From<&symbol::Symbol> for Symbol {
//    fn from(s: &symbol::Symbol) -> Symbol {
//        Symbol(SymbolIndex {
//            private: s.as_u32(),
//        })
//    }
//}

//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//struct SymbolIndex {
//    private: u32,
//}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct VariantIdx {
    private: u32,
}

impl From<&abi::VariantIdx> for VariantIdx {
    fn from(v: &abi::VariantIdx) -> VariantIdx {
        VariantIdx {
            private: v.as_u32(),
        }
    }
}
