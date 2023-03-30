use std::fmt::Debug;
use std::ops::Index;
use std::slice::SliceIndex;
use std::{collections::HashMap};

use crate::{
    abs::{
        backend::{OutputGenerator, PathInterestChecker, SolveResult, Solver, TraceManager},
        Constraint,
    },
    outgen::LoggerOutputGenerator,
    pathics::AllPathInterestChecker,
};

pub(crate) struct ImmediateTraceManager<S, V, I> {
    trace: Vec<S>,
    constraints: Vec<Constraint<V>>,
    path_interest_checker: Box<dyn PathInterestChecker<S>>,
    solver: Box<dyn Solver<Value = V, SymVarId = I>>,
    check_optimistic: bool,
    output_generator: Box<dyn OutputGenerator<I, V>>,
}

impl<S, V, I> ImmediateTraceManager<S, V, I> {
    pub fn new(
        path_interest_checker: Box<dyn PathInterestChecker<S>>,
        solver: Box<dyn Solver<Value = V, SymVarId = I>>,
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
impl<S, V: Debug, I: Debug> ImmediateTraceManager<S, V, I> {
    pub fn new_basic() -> Self {
        Self::new(
            Box::new(AllPathInterestChecker),
            todo!(),
            true,
            Box::new(LoggerOutputGenerator),
        )
    }
}

impl<S, V, I> TraceManager<S, V> for ImmediateTraceManager<S, V, I> {
    fn notify_step(&mut self, step: S, new_constraints: Vec<Constraint<V>>) {
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
             * Consider two independent if statements at the same level
             * that the current execution has taken neither.
             * Even if we satisfy the condition for the last one, we
             * can make a change in the path taken and we do not
             * necessary need to not satisfy the previous one.
             */
            if self.check_optimistic {
                self.check(self.constraints.len() - 1..);
            }
        }

        let last = self.constraints.pop().unwrap();
        self.constraints.push(last.not());
    }
}

impl<S, V, I> ImmediateTraceManager<S, V, I> {
    fn check(&mut self, range: impl SliceIndex<[Constraint<V>], Output = [Constraint<V>]>) -> bool {
        let result = self.solver.check(self.constraints.index(range));
        match result {
            SolveResult::Sat(values) => {
                self.generate_output(values);
                true
            }
            _ => false,
        }
    }

    fn generate_output(&mut self, values: HashMap<I, V>) {
        self.output_generator.generate(values.iter().collect());
    }
}
