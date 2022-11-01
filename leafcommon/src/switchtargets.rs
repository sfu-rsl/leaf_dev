extern crate rustc_middle;

use rustc_middle::mir;
use serde::{Deserialize, Serialize};
use std::{
    convert::From,
    fmt::{self, Display, Formatter},
    result,
};

/// #[derive(Debug, Clone, TyEncodable, TyDecodable, Hash, HashStable, PartialEq, PartialOrd)]
// pub struct SwitchTargets {
//     /// Possible values. The locations to branch to in each case
//     /// are found in the corresponding indices from the `targets` vector.
//     values: SmallVec<[u128; 1]>,
//
//     /// Possible branch sites. The last element of this vector is used
//     /// for the otherwise branch, so targets.len() == values.len() + 1
//     /// should hold.
//     //
//     // This invariant is quite non-obvious and also could be improved.
//     // One way to make this invariant is to have something like this instead:
//     //
//     // branches: Vec<(ConstInt, BasicBlock)>,
//     // otherwise: Option<BasicBlock> // exhaustive if None
//     //
//     // However we’ve decided to keep this as-is until we figure a case
//     // where some other approach seems to be strictly better than other.
//     targets: SmallVec<[BasicBlock; 2]>,
// }

/// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/terminator/struct.SwitchTargets.html
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct SwitchTargets {
    /// Vector containg tuples containing value and corresponding basic block index target
    pub switch_targets: Vec<(u128, u32)>,
    /// Possible branch sites (basic block indices). The last element of this vector is used for the
    /// otherwise branch, so targets.len() == values.len() + 1 should hold.
    pub otherwise: Option<u32>,
}

impl Display for SwitchTargets {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl From<&mir::terminator::SwitchTargets> for SwitchTargets {
    fn from(st: &mir::terminator::SwitchTargets) -> SwitchTargets {
        let switch_targets: Vec<(u128, u32)> = st
            .iter()
            .map(|(value, bb)| (value, bb.as_u32()))
            .collect::<Vec<(u128, u32)>>();
        let otherwise: Option<u32> = Some(st.otherwise().as_u32());

        SwitchTargets {
            switch_targets,
            otherwise,
        }
    }
}

impl TryFrom<&str> for SwitchTargets {
    type Error = serde_json::Error;

    fn try_from(s: &str) -> result::Result<Self, Self::Error> {
        serde_json::from_str(s)
    }
}
