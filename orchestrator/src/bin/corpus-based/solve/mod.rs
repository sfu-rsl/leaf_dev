mod solvers;

use std::{collections::HashMap, path::Path, sync::Arc};

use futures::{Stream, StreamExt};

use crate::{HasBaseBytes, HasByteAnswers, trace::TraceConstraint};

use self::solvers::SolverImpl;

pub(crate) trait SolveQuery {
    type Constraint;

    fn into_constraints(self) -> impl Iterator<Item = Self::Constraint> + Send;
}

pub(crate) trait IntoQuery {
    type Constraint;
    type Query: SolveQuery<Constraint = Self::Constraint>;

    fn into_query(self) -> Self::Query;
}

enum QueryResult {
    Satisfied(HashMap<usize, u8>),
    Unsatisfied,
}

trait Solver {
    type Constraint;

    async fn solve<Q>(&mut self, query: Q) -> QueryResult
    where
        Q: SolveQuery<Constraint = Self::Constraint>;
}

pub(crate) trait HasByteInput {
    fn input_bytes(&self) -> Arc<[u8]>;
}

mod derived_output {
    use super::*;

    pub(crate) struct DerivedOutput {
        input_bytes: Arc<[u8]>,
        answers: Vec<(usize, Option<u8>)>,
    }

    impl DerivedOutput {
        pub(super) fn new(input_bytes: Arc<[u8]>, answers: Vec<(usize, Option<u8>)>) -> Self {
            Self {
                input_bytes,
                answers,
            }
        }
    }

    impl HasBaseBytes for DerivedOutput {
        fn base_bytes(&self) -> impl AsRef<[u8]> + Send + 'static {
            self.input_bytes.clone()
        }
    }

    impl HasByteAnswers for DerivedOutput {
        fn answers<'a>(&'a self) -> impl ExactSizeIterator<Item = (usize, Option<u8>)> + Send + 'a {
            self.answers.iter().cloned()
        }
    }
}

type Output = derived_output::DerivedOutput;

mod outgen {
    use super::*;

    pub(super) fn generate_output(
        query_result: QueryResult,
        input_bytes: Arc<[u8]>,
    ) -> Option<Output> {
        let answers = match query_result {
            QueryResult::Satisfied(hash_map) => hash_map,
            QueryResult::Unsatisfied => return None,
        };

        Some(Output::new(
            input_bytes,
            answers.into_iter().map(|(id, v)| (id, Some(v))).collect(),
        ))
    }
}

pub(crate) fn realize_potentials<P, S>(
    solver_bin_path: &Path,
    workdir: &Path,
    potentials: S,
) -> impl Stream<Item = Output> + Send + use<P, S>
where
    P: Send,
    P: HasByteInput,
    P: IntoQuery<Constraint = TraceConstraint> + Send,
    P::Query: Send,
    // Ensure interface compatibility
    Output: HasBaseBytes + HasByteAnswers + Send,
    S: Stream<Item = P> + Send,
{
    let solver = SolverImpl::new(solver_bin_path, workdir.join("solve"));

    potentials
        .map(move |p| (solver.clone(), p))
        .filter_map(async move |(mut solver, p)| {
            let input = p.input_bytes();
            let result = solver.solve(p.into_query()).await;
            outgen::generate_output(result, input)
        })
}
