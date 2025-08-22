use std::sync::Arc;

use futures::{Stream, StreamExt};
use indicatif::ProgressBar;

use crate::{HasByteInput, IntoQuery, SolveQuery, TraceConstraint, TraceSwitchStep};

pub(crate) type SwitchTrace = Vec<TraceSwitchStep>;

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

        fn into_constraints(self) -> impl Iterator<Item = Self::Constraint> {
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

            let influenceable_steps = trace.switches.iter().enumerate().filter(|(_, step)| {
                step.constraint.is_some() || !step.implied_by_offset.is_empty()
            });

            let all_constraints = trace
                .switches
                .iter()
                .map(|step| step.constraint.clone())
                .collect::<Vec<_>>();

            let all_constraints: ConstraintsRef = Arc::from(all_constraints.into_boxed_slice());

            // Additionally, we filter out steps that are only influenceable implicitly for simplicity.
            let directly_influenceable_step_indices = influenceable_steps
                .into_iter()
                .filter(|(_, step)| step.constraint.is_some())
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
        pb: ProgressBar,
    }

    impl NoOpCorpusPotentialPrioritizer {
        pub fn new(pb: ProgressBar) -> (Self, impl Stream<Item = Potential>) {
            let (tx, rx) = priority_channel::channel::<NoOrderPotential>();
            (
                Self {
                    potentials_tx: tx,
                    pb,
                },
                rx.to_stream().map(|potential| potential.0),
            )
        }

        pub async fn prioritize(&self, potentials: impl Stream<Item = Potential>) {
            potentials
                .for_each(async |potential| {
                    let no_ordered = NoOrderPotential(potential);
                    self.pb.inc_length(1);
                    self.potentials_tx.send(no_ordered).await;
                })
                .await;
        }
    }
}

pub(crate) fn prioritized_potential(
    traces: impl Stream<Item = Trace> + Send + 'static,
    pb: ProgressBar,
) -> (tokio::task::JoinHandle<()>, impl Stream<Item = Potential> + 'static)
where
    // Ensure interfaces are compatible
    Potential: HasByteInput,
    Potential: IntoQuery<Constraint = TraceConstraint> + Send,
    <Potential as IntoQuery>::Query: Send,
{
    let (prioritizer, prioritized_potentials) =
        priority::NoOpCorpusPotentialPrioritizer::new(pb.clone());

    let process_handle = tokio::task::spawn(async move {
        let potentials = collect::assess_traces(traces);
        prioritizer.prioritize(potentials).await;
    });

    (process_handle, prioritized_potentials)
}
