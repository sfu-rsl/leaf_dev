use core::iter;

use common::{log_debug, log_info, log_warn};

use crate::abs::{
    backend::{SolveResult, Solver},
    Constraint,
};

use super::TraceInspector;

pub(crate) trait DivergenceFilter<S> {
    fn should_find(&mut self, trace: &[S]) -> bool;
}

impl<S, T: FnMut(&[S]) -> bool> DivergenceFilter<S> for T {
    fn should_find(&mut self, trace: &[S]) -> bool {
        self(trace)
    }
}

pub(crate) struct ImmediateDivergingAnswerFinder<S: Solver, TS, C> {
    solver: S,
    filter: C,
    check_optimistic: bool,
    model_consumer: Box<dyn FnMut(S::Model)>,
    _phantom: core::marker::PhantomData<TS>,
}

impl<S: Solver, TS, C: DivergenceFilter<TS>> TraceInspector<TS, S::Value>
    for ImmediateDivergingAnswerFinder<S, TS, C>
where
    S::Value: Clone,
{
    fn inspect(&mut self, steps: &[TS], constraints: &[Constraint<S::Value>]) {
        // Sanity check
        #[cfg(debug_assertions)]
        if !self.check(constraints.iter(), false) {
            log_warn!("Unsatisfiable concretely taken path");
        }

        if !self.filter.should_find(steps) {
            return;
        }

        log_debug!("Negating the last constraint");
        let not_last = constraints.last().cloned().unwrap().not();

        if !self.check(
            constraints[..constraints.len() - 1]
                .into_iter()
                .chain(iter::once(&not_last)),
            true,
        ) {
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
                self.check(iter::once(&not_last), true);
            }
        }
    }
}

impl<S: Solver, TS, C> ImmediateDivergingAnswerFinder<S, TS, C> {
    pub fn new(
        solver: S,
        filter: C,
        check_optimistic: bool,
        model_consumer: Box<dyn FnMut(S::Model)>,
    ) -> Self {
        Self {
            solver,
            filter,
            check_optimistic,
            model_consumer,
            _phantom: Default::default(),
        }
    }
}

impl<S: Solver, TS, C> ImmediateDivergingAnswerFinder<S, TS, C> {
    pub(crate) fn check<'a>(
        &mut self,
        constraints: impl Iterator<Item = &'a Constraint<S::Value>>,
        gen_output: bool,
    ) -> bool
    where
        S: 'a,
    {
        let result = self.solver.check(constraints);
        match result {
            SolveResult::Sat(model) => {
                if gen_output {
                    (self.model_consumer)(model);
                }
                true
            }
            _ => {
                log_info!("Unsatisfiable or unknown result.");
                false
            }
        }
    }
}
