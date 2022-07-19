extern crate rustc_middle;

use rustc_middle::mir;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};
use std::{fmt, result};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DebugInfo {
    pub variable_name: Option<String>,
}

impl Display for DebugInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl<'tcx> From<&mir::VarDebugInfo<'tcx>> for DebugInfo {
    fn from(info: &mir::VarDebugInfo) -> DebugInfo {
        DebugInfo {
            variable_name: Some(info.name.to_string()),
        }
    }
}

impl TryFrom<&str> for DebugInfo {
    type Error = serde_json::Error;

    fn try_from(s: &str) -> result::Result<Self, Self::Error> {
        serde_json::from_str(s)
    }
}
