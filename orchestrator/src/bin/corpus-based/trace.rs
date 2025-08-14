use common::{
    directed::RawCaseValue,
    pri::BasicBlockLocation,
    types::trace::{Constraint, ConstraintKind},
};

use derive_more as dm;
use serde::{Deserialize, Serialize};

type OpaqueExpression = common::z3::serdes::SmtLibExpr;

#[derive(Debug, Clone, Serialize, Deserialize, dm::Into)]
#[serde(transparent)]
pub(crate) struct TraceCaseValue(OpaqueExpression);

#[derive(Debug, Clone, Serialize, Deserialize, dm::Display, dm::Into)]
#[serde(transparent)]
pub(crate) struct TraceValue(OpaqueExpression);

#[derive(Debug, Clone)]
pub(crate) struct TraceSwitchStep {
    pub trace_index: usize,
    pub location: BasicBlockLocation,
    pub decision: ConstraintKind<RawCaseValue>,
    pub constraint: Option<TraceConstraint>,
    pub implied_by_offset: Vec<usize>,
}

pub(crate) type TraceConstraint = Constraint<TraceValue, TraceCaseValue>;

impl core::fmt::Display for TraceSwitchStep {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "@{}: {}", self.location, Constraint {
            discr: self
                .constraint
                .as_ref()
                .map(|c| c.discr.to_string())
                .unwrap_or("<Conc>".to_owned()),
            kind: self.decision.as_ref(),
        })
    }
}
