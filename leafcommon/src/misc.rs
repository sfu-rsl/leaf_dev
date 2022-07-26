extern crate rustc_middle;

use crate::place::Place;
use rustc_middle::mir;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};
use std::{fmt, result};

/// Coupling of place and debug info together in order to be able to serialize/deserialize Options
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct PlaceAndDebugInfo {
    pub place: Option<Place>,
    pub debug_info: Option<DebugInfo>,
}

impl Display for PlaceAndDebugInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl TryFrom<&str> for PlaceAndDebugInfo {
    type Error = serde_json::Error;

    fn try_from(s: &str) -> result::Result<Self, Self::Error> {
        serde_json::from_str(s)
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct DebugInfo {
    pub name: Option<String>,
}

impl Display for DebugInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl<'tcx> From<&mir::VarDebugInfo<'tcx>> for DebugInfo {
    fn from(info: &mir::VarDebugInfo) -> DebugInfo {
        DebugInfo {
            name: Some(info.name.to_string()),
        }
    }
}

impl TryFrom<&str> for DebugInfo {
    type Error = serde_json::Error;

    fn try_from(s: &str) -> result::Result<Self, Self::Error> {
        serde_json::from_str(s)
    }
}
