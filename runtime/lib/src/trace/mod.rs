use std::ops::Index;
use std::slice::SliceIndex;
use std::{collections::HashMap, fmt::Display};

use crate::abs::{
    backend::{PathInterestChecker, SolveResult, Solver, TraceManager},
    Constraint,
};

use common::{log_debug, log_info};

pub(crate) struct ImmediateTraceManager<Step, Id, Val, Output> {
    trace: Vec<Step>,
    constraints: Vec<Constraint<Val>>,
    path_interest_checker: Box<dyn PathInterestChecker<Step>>,
    solver: Box<dyn Solver<Id, Val>>,
    check_optimistic: bool,
    answer_consumer: Box<dyn FnMut(HashMap<Id, Val>) -> Output>,
}

impl<S, I, V, O> ImmediateTraceManager<S, I, V, O> {
    pub fn new(
        path_interest_checker: Box<dyn PathInterestChecker<S>>,
        solver: Box<dyn Solver<I, V>>,
        check_optimistic: bool,
        output_generator: Box<dyn FnMut(HashMap<I, V>) -> O>,
    ) -> Self {
        Self {
            trace: Vec::new(),
            constraints: Vec::new(),
            path_interest_checker,
            solver,
            check_optimistic,
            answer_consumer: output_generator,
        }
    }
}

impl<S: Display, I, V: Display, O> TraceManager<S, V> for ImmediateTraceManager<S, I, V, O> {
    fn notify_step(&mut self, step: S, new_constraints: Vec<Constraint<V>>) {
        log_info!(
            "Notified about constraints [{}] at step {}",
            &new_constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            step,
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

        log_debug!("Negating the last constraint");
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
                log_debug!("Checking optimistically using the last constraint");
                self.check(self.constraints.len() - 1..);
            }
        }

        let last = self.constraints.pop().unwrap();
        self.constraints.push(last.not());
    }
}

impl<S, I, V: Display, O> ImmediateTraceManager<S, I, V, O> {
    fn check(&mut self, range: impl SliceIndex<[Constraint<V>], Output = [Constraint<V>]>) -> bool {
        let constraints = self.constraints.index(range);

        log_debug!(
            "Sending constraints to the solver: [\n{}\n]",
            constraints
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(",\n")
        );

        let result = self.solver.check(constraints);
        match result {
            SolveResult::Sat(values) => {
                self.generate_output(values);
                true
            }
            _ => {
                log_info!("Unsatisfiable or unknown result.");
                false
            }
        }
    }

    fn generate_output(&mut self, values: HashMap<I, V>) -> O {
        (self.answer_consumer)(values)
    }
}
