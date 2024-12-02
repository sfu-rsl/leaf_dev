use crate::abs::{backend::TraceManager, Constraint};

mod adapt;
mod agg;
pub(crate) mod divergence;
mod filter;
pub(crate) mod inspect;
mod log;
pub(crate) mod sanity_check;

pub(crate) use adapt::TraceManagerExt as AdapterTraceManagerExt;
pub(crate) use agg::AggregatorTraceManager;
pub(crate) use inspect::TraceInspector;
pub(crate) use log::TraceManagerExt as LoggerTraceManagerExt;
