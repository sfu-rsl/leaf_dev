extern crate rustc_middle;

use crate::place::{Local, Place, VariantIdx};
use crate::rvalue::Rvalue;
use rustc_middle::mir;
use serde::{Deserialize, Serialize};

/// See https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/enum.StatementKind.html
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum StatementKind {
    Assign(Place, Rvalue),
    SetDiscriminant {
        place: Place,
        variant_index: VariantIdx,
    },
    StorageLive(Local),
    StorageDead(Local),
    Unused,
}

impl<'tcx> From<&mir::StatementKind<'tcx>> for StatementKind {
    fn from(s: &mir::StatementKind) -> StatementKind {
        match s {
            mir::StatementKind::Assign(b) => {
                let (place, rvalue): &(mir::Place<'_>, mir::Rvalue<'_>) = &*b;
                StatementKind::Assign(place.into(), rvalue.into())
            }
            mir::StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => StatementKind::SetDiscriminant {
                place: (&**place).into(),
                variant_index: variant_index.into(),
            },
            mir::StatementKind::StorageLive(l) => StatementKind::StorageLive(l.into()),
            mir::StatementKind::StorageDead(l) => StatementKind::StorageDead(l.into()),
            _ => StatementKind::Unused,
        }
    }
}
