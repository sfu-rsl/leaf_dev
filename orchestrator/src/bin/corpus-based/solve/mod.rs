use std::{collections::HashMap, sync::Arc};

use futures::{Stream, StreamExt};

use common::types::trace::Constraint;

use crate::{HasBaseBytes, HasByteAnswers};

pub(crate) trait SolveQuery {
    type Constraint;

    fn into_constraints<'a>(self) -> impl Iterator<Item = Self::Constraint> + 'a
    where
        Self: 'a;
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

mod solve {
    use super::*;

    mod z3 {
        use ::z3::SatResult;

        use common::{
            directed::RawCaseValue,
            z3::{
                AstAndVars, AstNode, AstNodeSort, BVNode, BVSort, WrappedSolver, serdes::SmtLibExpr,
            },
        };

        type TraceConstraint = Constraint<SmtLibExpr, RawCaseValue>;
        type Z3Constraint<'ctx> = Constraint<AstAndVars<'ctx, u32>, AstNode<'ctx>>;

        use super::*;

        #[derive(Clone)]
        pub(crate) struct Adapter<'ctx> {
            _phantom: std::marker::PhantomData<&'ctx ()>,
        }

        impl<'ctx> Adapter<'ctx> {
            pub fn new() -> Self {
                Self {
                    _phantom: Default::default(),
                }
            }
        }

        impl<'ctx> Solver for Adapter<'ctx> {
            type Constraint = TraceConstraint;

            async fn solve<Q>(&mut self, query: Q) -> QueryResult
            where
                Q: SolveQuery<Constraint = Self::Constraint>,
            {
                // FIXME: Temporarily creating and parsing repeatedly until we refactor the solver
                let solver = WrappedSolver::new_in_global_context();
                let (solver_result, answers) = solver.check(query.into_constraints().map(|c| {
                    let discr = c.discr.parse(solver.context(), &mut Default::default());
                    Z3Constraint {
                        kind: c.kind.clone().map(ast_mapper(&discr)),
                        discr,
                    }
                }));
                match solver_result {
                    SatResult::Sat => QueryResult::Satisfied(
                        answers
                            .iter()
                            .map(|(id, answer)| (*id as usize - 1, try_ast_to_byte(answer)))
                            .flat_map(|(index, byte)| byte.map(|b| (index, b)))
                            .collect(),
                    ),
                    SatResult::Unsat | SatResult::Unknown => QueryResult::Unsatisfied,
                }
            }
        }

        fn try_ast_to_byte(ast: &AstNode) -> Option<u8> {
            match ast {
                AstNode::BitVector(BVNode(bv, BVSort { is_signed: false })) => (bv.get_size()
                    == u8::BITS)
                    .then(|| bv.as_u64())
                    .flatten()
                    .map(|x| x as u8),
                _ => None,
            }
        }

        fn ast_mapper<'ctx, C: std::borrow::Borrow<u128>>(
            discr: &AstNode<'ctx>,
        ) -> impl FnMut(C) -> AstNode<'ctx> {
            let sort = discr.sort();

            move |case| {
                AstNode::BitVector(BVNode(
                    ::z3::ast::BV::from_str(
                        discr.ast().get_ctx(),
                        discr.as_bit_vector().get_size(),
                        &case.borrow().to_string(),
                    )
                    .unwrap(),
                    {
                        let AstNodeSort::BitVector(sort) = sort else {
                            unreachable!("Unexpected sort for a non-bool discriminant: {:?}", sort)
                        };
                        sort
                    },
                ))
            }
        }
    }

    pub(super) type SolverImpl = z3::Adapter<'static>;
    // pub(super) type SolverConstraint = <SolverImpl as Solver>::Constraint;
    pub(super) type SolverConstraint =
        Constraint<common::z3::serdes::SmtLibExpr, common::directed::RawCaseValue>;
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

pub(crate) fn realize_potentials<P>(
    potentials: impl Stream<Item = P> + Send,
) -> impl Stream<Item = Output> + Send
where
    P: Send,
    P: HasByteInput,
    P: IntoQuery<Constraint = solve::SolverConstraint>,
    P::Query: Send,
    // Ensure interface compatibility
    Output: HasBaseBytes + HasByteAnswers + Send,
{
    let solver = solve::SolverImpl::new();

    potentials
        .map(move |p| (solver.clone(), p))
        .filter_map(async move |(mut solver, p)| {
            let input = p.input_bytes();
            let result = solver.solve(p.into_query()).await;
            outgen::generate_output(result, input)
        })
}
