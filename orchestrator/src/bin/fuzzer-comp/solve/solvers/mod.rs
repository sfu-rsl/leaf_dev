mod leafsolver;
mod z3;

use super::{QueryResult, SolveQuery, Solver, TraceConstraint};

// pub(super) type SolverImpl = z3::Adapter<'static>;
pub(super) type SolverImpl = leafsolver::Adapter;
