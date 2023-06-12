use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::Index;
use std::slice::SliceIndex;

use crate::{
    abs::{
        backend::{OutputGenerator, PathInterestChecker, SolveResult, Solver, TraceManager},
        Constraint,
    },
    outgen::LoggerOutputGenerator,
    pathics::AllPathInterestChecker,
    utils::logging::{log_debug, log_info},
};

pub(crate) struct ImmediateTraceManager<Step, Id, Val> {
    trace: Vec<Step>,
    constraints: Vec<Constraint<Val>>,
    path_interest_checker: Box<dyn PathInterestChecker<Step>>,
    solver: Box<dyn Solver<Id, Val>>,
    check_optimistic: bool,
    output_generator: Box<dyn OutputGenerator<Id, Val>>,
}

impl<S, I, V> ImmediateTraceManager<S, I, V> {
    pub fn new(
        path_interest_checker: Box<dyn PathInterestChecker<S>>,
        solver: Box<dyn Solver<I, V>>,
        check_optimistic: bool,
        output_generator: Box<dyn OutputGenerator<I, V>>,
    ) -> Self {
        Self {
            trace: Vec::new(),
            constraints: Vec::new(),
            path_interest_checker,
            solver,
            check_optimistic,
            output_generator,
        }
    }
}
impl<S, I: Debug, V: Debug> ImmediateTraceManager<S, I, V> {
    pub fn new_basic(solver: Box<dyn Solver<I, V>>) -> Self {
        Self::new(
            Box::new(AllPathInterestChecker),
            solver,
            true,
            Box::new(LoggerOutputGenerator),
        )
    }
}

impl<S: Debug, I, V: Debug> TraceManager<S, V> for ImmediateTraceManager<S, I, V> {
    fn notify_step(&mut self, step: S, new_constraints: Vec<Constraint<V>>) {
        log_info!(
            "Took step: {:?} with constraints {:?}",
            step,
            &new_constraints
        );

        self.trace.push(step);

        if new_constraints.is_empty() {
            return;
        }

        self.constraints.extend(new_constraints);

        if !self
            .path_interest_checker
            .is_interesting(self.trace.as_slice())
        {
            return;
        }

        let last = self.constraints.pop().unwrap();
        self.constraints.push(last.not());

        if !self.check(..) {
            /* NOTE: What is optimistic checking?
             * Consider two independent branch conditions at the same level
             * that the current execution has taken neither.
             * Even if we satisfy the condition for the second one, we
             * have a chance to make a change in the execution path.
             * Thus we do not necessary need to satisfy the constraints for the
             * first one.
             */
            if self.check_optimistic {
                self.check(self.constraints.len() - 1..);
            }
        }

        let last = self.constraints.pop().unwrap();
        self.constraints.push(last.not());
    }
}

impl<S, I, V> ImmediateTraceManager<S, I, V> {
    fn check(&mut self, range: impl SliceIndex<[Constraint<V>], Output = [Constraint<V>]>) -> bool {
        let result = self.solver.check(self.constraints.index(range));
        match result {
            SolveResult::Sat(values) => {
                self.generate_output(values);
                true
            }
            _ => {
                log_debug!("Unsatisfiable or unknown result.");
                false
            }
        }
    }

    fn generate_output(&mut self, values: HashMap<I, V>) {
        self.output_generator.generate(values.iter().collect());
    }
}
