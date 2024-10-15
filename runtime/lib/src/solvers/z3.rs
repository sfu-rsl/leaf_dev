use derive_more as dm;
use lazy_static::lazy_static;
use z3::{
    self,
    ast::{self, Ast},
    Config, Context, SatResult, Solver,
};

use std::{borrow::Borrow, collections::HashMap, hash::Hash};

use common::log_debug;

use crate::{abs::backend, utils::UnsafeSync};

/* NOTE: Why not using `Dynamic`?
 * In this way we have a little more freedom to include our information such
 * as whether the bit vector is signed or not.
 */
#[derive(Debug, Clone)]
pub(crate) enum AstNode<'ctx> {
    Bool(ast::Bool<'ctx>),
    BitVector(BVNode<'ctx>),
    Array(ArrayNode<'ctx>),
}

impl<'ctx> From<BVNode<'ctx>> for AstNode<'ctx> {
    fn from(node: BVNode<'ctx>) -> Self {
        Self::BitVector(node)
    }
}

impl<'ctx> From<ArrayNode<'ctx>> for AstNode<'ctx> {
    fn from(node: ArrayNode<'ctx>) -> Self {
        Self::Array(node)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BVNode<'ctx>(pub ast::BV<'ctx>, pub BVSort);

impl<'ctx> BVNode<'ctx> {
    pub fn new(ast: ast::BV<'ctx>, is_signed: bool) -> Self {
        Self(ast, BVSort { is_signed })
    }

    #[inline]
    pub(crate) fn map<F>(&self, f: F) -> Self
    where
        F: FnOnce(&ast::BV<'ctx>) -> ast::BV<'ctx>,
    {
        Self(f(&self.0), self.1)
    }

    #[inline(always)]
    pub(crate) fn is_signed(&self) -> bool {
        self.1.is_signed
    }

    #[inline(always)]
    pub(crate) fn size(&self) -> u32 {
        self.0.get_size()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ArrayNode<'ctx>(pub ast::Array<'ctx>, pub ArraySort);

#[derive(Debug, Clone, PartialEq, dm::From)]
pub(crate) enum AstNodeSort {
    Bool,
    BitVector(BVSort),
    Array(ArraySort),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct BVSort {
    pub is_signed: bool,
}

#[derive(Debug, Clone, PartialEq, dm::From)]
pub(crate) struct ArraySort {
    pub range: Box<AstNodeSort>,
}

impl<'ctx> From<ast::Bool<'ctx>> for AstNode<'ctx> {
    fn from(ast: ast::Bool<'ctx>) -> Self {
        Self::Bool(ast)
    }
}

impl<'ctx> AstNode<'ctx> {
    pub fn from_ubv(ast: ast::BV<'ctx>) -> Self {
        BVNode::new(ast, false).into()
    }

    pub fn from_ast(ast: ast::Dynamic<'ctx>, sort: &AstNodeSort) -> Self {
        match sort {
            AstNodeSort::Bool => ast.as_bool().map(Self::Bool),
            AstNodeSort::BitVector(sort) => {
                ast.as_bv().map(|ast| Self::BitVector(BVNode(ast, *sort)))
            }
            AstNodeSort::Array(sort) => ast
                .as_array()
                .map(|ast| Self::Array(ArrayNode(ast, sort.clone()))),
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
            Self::BitVector(BVNode(ast, _)) => ast,
            _ => panic!("Expected the value to be a bit vector."),
        }
    }
}

impl<'ctx> AstNode<'ctx> {
    pub fn ast(&self) -> ast::Dynamic<'ctx> {
        ast::Dynamic::from_ast(match self {
            Self::Bool(ast) => ast,
            Self::BitVector(BVNode(ast, _)) => ast,
            Self::Array(ArrayNode(ast, _)) => ast,
        })
    }

    pub fn sort(&self) -> AstNodeSort {
        match self {
            Self::Bool(_) => AstNodeSort::Bool,
            Self::BitVector(BVNode(_, sort)) => AstNodeSort::BitVector(*sort),
            Self::Array(ArrayNode(_, sort)) => AstNodeSort::Array(sort.clone()),
        }
    }

    pub fn z3_sort(&self) -> z3::Sort<'ctx> {
        match self {
            Self::Bool(ast) => ast.get_sort(),
            Self::BitVector(BVNode(ast, _)) => ast.get_sort(),
            Self::Array(ArrayNode(ast, _)) => ast.get_sort(),
        }
    }
}

pub(crate) struct TranslatedConstraint<'ctx, I> {
    pub constraint: ast::Bool<'ctx>,
    pub variables: HashMap<I, AstNode<'ctx>>,
    pub extra: Vec<ast::Bool<'ctx>>,
}

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

pub(crate) struct Z3Solver<'ctx, Id, Val, Translator> {
    context: &'ctx Context,
    solver: Option<Solver<'ctx>>,
    translator: Translator,
    _phantom: std::marker::PhantomData<(Id, Val)>,
}

impl<'ctx, I, V, T> Z3Solver<'ctx, I, V, T> {
    pub fn new_in_global_context(translator_factory: impl FnOnce(&'ctx Context) -> T) -> Self {
        let context = CONTEXT.borrow();
        Self {
            context,
            solver: None,
            translator: translator_factory(context),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<'ctx, I, V, T> backend::Solver<I, V> for Z3Solver<'ctx, I, V, T>
where
    I: Eq + Hash,
    V: From<AstNode<'ctx>>,
    T: for<'v> FnMut(&'v V) -> TranslatedConstraint<'ctx, I>,
    Self: 'ctx,
{
    fn check(&mut self, constraints: &[crate::abs::Constraint<V>]) -> backend::SolveResult<I, V> {
        self.solver.get_or_insert_with(|| Solver::new(self.context));

        let mut all_vars = HashMap::<I, AstNode>::new();
        let asts = constraints
            .iter()
            .flat_map(|constraint| {
                let (value, is_negated) = constraint.destruct_ref();
                let TranslatedConstraint {
                    constraint: ast,
                    variables,
                    extra,
                } = (self.translator)(value);
                all_vars.extend(variables);

                let constraint = if is_negated { ast.not() } else { ast };

                extra.into_iter().chain(std::iter::once(constraint))
            })
            .collect::<Vec<_>>();

        let result = self.check_using(self.solver.as_ref().unwrap(), &asts, all_vars);
        result
    }
}

impl<'ctx, I, V, T> Z3Solver<'_, I, V, T>
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
        log_debug!("Sending constraints to Z3: {:#?}", constraints);

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
                        AstNode::BitVector(BVNode(ast, is_signed)) => {
                            AstNode::BitVector(BVNode(model.eval(&ast, true).unwrap(), is_signed))
                        }
                        AstNode::Array(ArrayNode(ast, sort)) => {
                            AstNode::Array(ArrayNode(model.eval(&ast, true).unwrap(), sort))
                        }
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

pub(crate) trait BVExt {
    fn as_u128(&self) -> Option<u128>;
}

impl<'ctx> BVExt for ast::BV<'ctx> {
    fn as_u128(&self) -> Option<u128> {
        if self.get_size() <= 128 {
            unsafe {
                use std::ffi::CStr;
                Some(z3_sys::Z3_get_numeral_string(
                    self.get_ctx().get_z3_context(),
                    self.get_z3_ast(),
                ))
                .filter(|x| !x.is_null())
                .map(|x| CStr::from_ptr(x))
                .and_then(|s| s.to_str().ok())
                .and_then(|s| u128::from_str_radix(s, 10).ok())
            }
        } else {
            None
        }
    }
}
