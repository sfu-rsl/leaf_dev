use ::z3::SatResult;

use common::z3::{AstNode, AstNodeSort, BVNode, BVSort, WrappedSolver, serdes::SmtLibExpr};

use crate::trace::{TraceCaseValue, TraceValue};

use super::{QueryResult, SolveQuery, Solver, TraceConstraint};

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

impl<'ctx> Solver for Adapter<'ctx>
where
    TraceValue: Into<SmtLibExpr>,
    TraceCaseValue: Into<SmtLibExpr>,
{
    type Constraint = TraceConstraint;

    async fn solve<Q>(&mut self, query: Q) -> QueryResult
    where
        Q: SolveQuery<Constraint = Self::Constraint>,
    {
        // FIXME: Temporarily creating and parsing repeatedly until we refactor the solver
        let solver = WrappedSolver::<u32>::new_in_global_context();
        let (solver_result, answers) = solver.check(query.into_constraints().map(|c| {
            c.map(
                |d| Into::<SmtLibExpr>::into(d).parse(solver.context(), &mut Default::default()),
                |c| {
                    Into::<SmtLibExpr>::into(c)
                        .parse_as_const(solver.context())
                        .unwrap()
                },
            )
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
        AstNode::BitVector(BVNode(bv, BVSort { is_signed: false })) => (bv.get_size() == u8::BITS)
            .then(|| bv.as_u64())
            .flatten()
            .map(|x| x as u8),
        _ => None,
    }
}
