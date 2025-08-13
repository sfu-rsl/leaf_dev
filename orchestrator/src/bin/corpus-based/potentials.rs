use std::sync::Arc;

use futures::{Stream, StreamExt};

use common::{
    directed::RawCaseValue,
    types::{
        BasicBlockLocation,
        trace::{Constraint, ConstraintKind},
    },
    z3::serdes::SmtLibExpr,
};

use crate::{HasByteInput, IntoQuery, SolveQuery};

type Expression = SmtLibExpr;

#[derive(Debug, Clone)]
pub(crate) struct SwitchStep<V = Expression> {
    pub trace_index: usize,
    pub location: BasicBlockLocation,
    pub decision: ConstraintKind<RawCaseValue>,
    pub discr: Option<V>,
    pub implied_by_offset: Vec<usize>,
}

pub(crate) type SwitchTrace = Vec<SwitchStep>;

type TraceConstraint = Constraint<Expression, RawCaseValue>;
type ConstraintsRef = Arc<[Option<TraceConstraint>]>;

#[derive(Debug)]
pub(crate) struct Trace {
    pub switches: SwitchTrace,
    pub input_buf: Arc<[u8]>,
}

type TraceRef = Arc<Trace>;

mod last_diverging {
    use super::*;

    pub(crate) struct LastDivergingPotential {
        trace: TraceRef,
        all_constraints: ConstraintsRef,
        at: usize,
    }

    impl LastDivergingPotential {
        pub fn new(trace: TraceRef, all_constraints: ConstraintsRef, at: usize) -> Self {
            Self {
                trace,
                all_constraints,
                at,
            }
        }
    }

    pub(crate) struct LastDivergingPotentialQuery {
        all_constraints: ConstraintsRef,
        at: usize,
    }

    impl HasByteInput for LastDivergingPotential {
        fn input_bytes(&self) -> Arc<[u8]> {
            self.trace.input_buf.clone()
        }
    }

    impl SolveQuery for LastDivergingPotentialQuery {
        type Constraint = TraceConstraint;

        fn into_constraints<'a>(self) -> impl Iterator<Item = Self::Constraint> + 'a
        where
            Self: 'a,
        {
            let last = self.all_constraints[self.at].clone().unwrap();
            let all_constraints = self.all_constraints.clone();
            (0..self.at)
                .into_iter()
                .filter_map(move |i| all_constraints[i].clone())
                .chain(core::iter::once(last.not()))
        }
    }

    impl IntoQuery for LastDivergingPotential {
        type Constraint = TraceConstraint;
        type Query = LastDivergingPotentialQuery;

        fn into_query(self) -> Self::Query {
            LastDivergingPotentialQuery {
                all_constraints: self.all_constraints,
                at: self.at,
            }
        }
    }
}

type Potential = last_diverging::LastDivergingPotential;

mod collect {
    use super::*;

    pub(super) fn assess_traces(
        traces: impl Stream<Item = Trace>,
    ) -> impl Stream<Item = Potential> {
        traces.flat_map(|trace| {
            let trace = Arc::new(trace);

            let influenceable_steps = trace
                .switches
                .iter()
                .enumerate()
                .filter(|(_, step)| step.discr.is_some() || !step.implied_by_offset.is_empty());

            let all_constraints = trace
                .switches
                .iter()
                .map(|step| {
                    step.discr.clone().map(|d| Constraint {
                        discr: d,
                        kind: step.decision.clone(),
                    })
                })
                .collect::<Vec<_>>();

            let all_constraints: ConstraintsRef = Arc::from(all_constraints.into_boxed_slice());

            // Additionally, we filter out steps that are only influenceable implicitly for simplicity.
            let directly_influenceable_step_indices = influenceable_steps
                .into_iter()
                .filter(|(_, step)| step.discr.is_some())
                .map(|(i, _)| i)
                .collect::<Vec<_>>();

            futures::stream::iter(
                directly_influenceable_step_indices
                    .into_iter()
                    .map(move |i| Potential::new(trace.clone(), all_constraints.clone(), i)),
            )
        })
    }
}

mod priority {

    use crate::utils::{no_order_wrapper, priority_channel};

    use super::*;

    no_order_wrapper!(NoOrderPotential(Potential));

    pub(super) struct NoOpCorpusPotentialPrioritizer {
        potentials_tx: priority_channel::Sender<NoOrderPotential>,
    }

    impl NoOpCorpusPotentialPrioritizer {
        pub fn new() -> (Self, impl Stream<Item = Potential>) {
            let (tx, rx) = priority_channel::channel::<NoOrderPotential>();
            (
                Self { potentials_tx: tx },
                rx.to_stream().map(|potential| potential.0),
            )
        }

        pub async fn prioritize(&self, potentials: impl Stream<Item = Potential>) {
            potentials
                .for_each(async |potential| {
                    let no_ordered = NoOrderPotential(potential);
                    self.potentials_tx.send(no_ordered).await;
                })
                .await;
        }
    }
}

pub(crate) fn prioritized_potential(
    traces: impl Stream<Item = Trace> + Send + 'static,
) -> (tokio::task::JoinHandle<()>, impl Stream<Item = Potential>)
where
    // Ensure interfaces are compatible
    Potential: HasByteInput,
    Potential: IntoQuery<Constraint = TraceConstraint> + Send,
    <Potential as IntoQuery>::Query: Send,
{
    let (prioritizer, prioritized_potentials) = priority::NoOpCorpusPotentialPrioritizer::new();

    let process_handle = tokio::task::spawn(async move {
        let potentials = collect::assess_traces(traces);
        prioritizer.prioritize(potentials).await;
    });

    (process_handle, prioritized_potentials)
}
