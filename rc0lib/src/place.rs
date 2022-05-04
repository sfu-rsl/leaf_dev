extern crate rustc_middle;

use rustc_middle::mir;
use serde::{Deserialize, Serialize};
use std::convert::From;

/// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.Place.html
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Place {
    pub local: Local,
    pub projection: Vec<PlaceElem>,
}

impl<'tcx> From<&mir::Place<'tcx>> for Place {
    fn from(pl: &mir::Place) -> Place {
        Place {
            local: Local::from(pl.local),
            projection: vec![],
        }
    }
}

//impl From<&str> for Place {
//    fn from(s: &str) -> Place {
//        serde_json::from_str(s).unwrap()
//    }
//}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum PlaceElem {
    Deref,
    //Field(Field, Ty),
    Field(Field),
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
    Downcast(Option<Symbol>, VariantIdx),
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct Local {
    private: u32,
}

impl From<mir::Local> for Local {
    fn from(l: mir::Local) -> Local {
        Local {
            private: l.as_u32(),
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct Field {
    private: u32,
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct Symbol(SymbolIndex);

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
struct SymbolIndex {
    private: u32,
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct VariantIdx {
    private: u32,
}
