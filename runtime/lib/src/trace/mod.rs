use crate::abs::{Constraint, backend::TraceManager};

mod adapt;
mod agg;
mod coverage;
pub(crate) mod divergence;
mod dump;
mod filter;
pub(crate) mod inspect;
mod log;
pub(crate) mod sanity_check;

pub(crate) use adapt::TraceManagerExt as AdapterTraceManagerExt;
pub(crate) use agg::{AggregatorStepInspector, AggregatorTraceManager};
pub(crate) use coverage::BranchCoverageStepInspector;
pub(crate) use filter::{
    StepInspectorExt as FilterStepInspectorExt, TraceManagerExt as FilterTraceManagerExt,
};
pub(crate) use inspect::{
    StepInspector, TraceInspector, TraceManagerExt as InspectionTraceManagerExt,
};
pub(crate) use log::TraceManagerExt as LoggerTraceManagerExt;
