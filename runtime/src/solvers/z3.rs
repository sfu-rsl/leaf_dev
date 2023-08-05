use std::{borrow::Borrow, collections::HashMap, hash::Hash};

use crate::{abs::backend, utils::UnsafeSync};
use lazy_static::lazy_static;
use z3::{
    self,
    ast::{self, Ast},
    Config, Context, SatResult, Solver,
};

/* NOTE: Why not using `Dynamic`?
 * In this way we have a little more freedom to include our information such
 * as whether the bit vector is signed or not.
 */
#[derive(Debug, Clone)]
pub(crate) enum AstNode<'ctx> {
    Bool(ast::Bool<'ctx>),
    BitVector { ast: ast::BV<'ctx>, is_signed: bool },
}

impl<'ctx> From<ast::Bool<'ctx>> for AstNode<'ctx> {
    fn from(ast: ast::Bool<'ctx>) -> Self {
        Self::Bool(ast)
    }
}

impl<'ctx> AstNode<'ctx> {
    pub fn from_ubv(ast: ast::BV<'ctx>) -> Self {
        Self::from_bv(ast, false)
    }

    pub fn from_bv(ast: ast::BV<'ctx>, is_signed: bool) -> Self {
        Self::BitVector { ast, is_signed }
    }

    /// Wraps a dynamic AST in a (typed) `AstNode` using another instance that is known to have the
    /// same sort as the new one.
    pub fn from_ast(ast: ast::Dynamic<'ctx>, variant: &Self) -> Self {
        match variant {
            Self::Bool(_) => ast.as_bool().map(Self::Bool),
            Self::BitVector { is_signed, .. } => ast.as_bv().map(|ast| Self::BitVector {
                ast,
                is_signed: *is_signed,
            }),
        }
        .unwrap_or_else(|| {
            panic!(
                "Sort of ${:?} is not compatible with the expected one.",
                ast
            )
        })
    }
}

impl<'ctx> AstNode<'ctx> {
    pub fn as_bool(&self) -> &ast::Bool<'ctx> {
        match self {
            Self::Bool(ast) => ast,
            _ => panic!("Expected the value to be a boolean expression."),
        }
    }

    pub fn as_bit_vector(&self) -> &ast::BV<'ctx> {
        match self {
            Self::BitVector { ast, .. } => ast,
            _ => panic!("Expected the value to be a bit vector."),
        }
    }
}

impl<'ctx> AstNode<'ctx> {
    pub fn ast(&self) -> ast::Dynamic<'ctx> {
        ast::Dynamic::from_ast(match self {
            Self::Bool(ast) => ast,
            Self::BitVector { ast, .. } => ast,
        })
    }

    pub fn sort(&self) -> z3::Sort<'ctx> {
        match self {
            Self::Bool(ast) => ast.get_sort(),
            Self::BitVector { ast, .. } => ast.get_sort(),
        }
    }
}

pub(crate) struct AstPair<'ctx, I>(
    pub ast::Bool<'ctx>,
    pub HashMap<I, AstNode<'ctx>>,
    pub Vec<ast::Bool<'ctx>>,
);

lazy_static! {
    /* FIXME: Can we have a safer and still clean approach?
     * Before making changes, note that getting a reference to the context in the
     * Z3Solver has helped us to avoid the need for translator factory and be
     * able to generate solvers on demand.
     * Initial guess is that it should be possible to maintain the instance of
     * context inside the solver using interior mutability. RefCell was tried
     * and I wasn't successful.
     */
    static ref CONTEXT: UnsafeSync<Context> = UnsafeSync::new(Context::new(&Config::default()));
}

pub(crate) struct Z3Solver<'ctx, Id, Val> {
    context: &'ctx Context,
    solver: Option<Solver<'ctx>>,
    _phantom: std::marker::PhantomData<(Id, Val)>,
}

impl<'ctx, I, V> Z3Solver<'ctx, I, V> {
    pub fn new(context: &'ctx Context) -> Self {
        Self {
            context,
            solver: None,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn new_in_global_context() -> Self {
        let context = CONTEXT.borrow();
        Self::new(context)
    }
}

impl<'ctx, I, V> backend::Solver<I, V> for Z3Solver<'ctx, I, V>
where
    AstPair<'ctx, I>: for<'v> From<(&'v V, &'ctx Context)>,
    I: Eq + Hash,
    V: From<AstNode<'ctx>>,
    Self: 'ctx,
{
    fn check(&mut self, constraints: &[crate::abs::Constraint<V>]) -> backend::SolveResult<I, V> {
        self.solver.get_or_insert_with(|| Solver::new(self.context));

        let mut all_vars = HashMap::<I, AstNode>::new();
        let asts = constraints
            .iter()
            .flat_map(|constraint| {
                let (value, is_negated) = constraint.destruct_ref();
                let AstPair(ast, variables, additional_constraints) =
                    AstPair::from((value, self.context));
                all_vars.extend(variables);

                let constraint = if is_negated { ast.not() } else { ast };

                additional_constraints
                    .into_iter()
                    .chain(std::iter::once(constraint))
            })
            .collect::<Vec<_>>();

        let result = self.check_using(self.solver.as_ref().unwrap(), &asts, all_vars);
        result
    }
}

impl<'ctx, I, V> Z3Solver<'_, I, V>
where
    I: Eq + Hash,
    V: From<AstNode<'ctx>>,
{
    fn check_using(
        &self,
        solver: &Solver<'ctx>,
        constraints: &[ast::Bool<'ctx>],
        vars: HashMap<I, AstNode<'ctx>>,
    ) -> backend::SolveResult<I, V> {
        log::debug!("Sending constraints to Z3: {:#?}", constraints);

        solver.push();

        for constraint in constraints {
            solver.assert(constraint);
        }

        let result = match solver.check() {
            SatResult::Sat => {
                let model = solver.get_model().unwrap();
                let mut values = HashMap::new();
                for (id, node) in vars {
                    let value = match node {
                        AstNode::Bool(ast) => AstNode::Bool(model.eval(&ast, true).unwrap()),
                        AstNode::BitVector { ast, is_signed } => AstNode::BitVector {
                            ast: model.eval(&ast, true).unwrap(),
                            is_signed,
                        },
                    };
                    values.insert(id, value.into());
                }
                backend::SolveResult::Sat(values)
            }
            SatResult::Unsat => backend::SolveResult::Unsat,
            SatResult::Unknown => backend::SolveResult::Unknown,
        };

        solver.pop(1);
        result
    }
}
