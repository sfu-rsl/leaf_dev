use std::marker::PhantomData;

use super::*;

/// A builder that first tries to build an expression using the current builder,
/// and if it fails (does not support building it), it tries to build it using
/// the next builder.
///
/// The current builder should return a `Result` that either is the built
/// expression (as `Ok`) or the expressions passed to it (as `Err`).
///
/// This struct is useful for overriding certain functions with optimizations, for example.
pub(crate) struct ChainedExprBuilder<Current, Next, Expr, CurrentExpr: Into<Expr> = Expr> {
    pub(crate) current: Current,
    pub(crate) next: Next,
    _phantom: PhantomData<(Expr, CurrentExpr)>,
}

impl<C, N, E, CE> Default for ChainedExprBuilder<C, N, E, CE>
where
    C: Default,
    N: Default,
    CE: Into<E>,
{
    fn default() -> Self {
        Self {
            current: Default::default(),
            next: Default::default(),
            _phantom: Default::default(),
        }
    }
}

macro_rules! try_on_current_then_next {
    /* If there's a single argument, it's straightforward. */
    ($self:ident, $method:ident, $arg:ident $(,)?) => {
        try_on_current_then_next!($self, $method, ($arg), |$arg|)
    };
    /* If there are multiple arguments, we need to take both the ones that
     * are passed back and the regular arguments. */
    ($self:ident, $method:ident, ($($args:expr),+), |$($err_args:tt),+| $(,)?) => {
        $self.current
            .$method($($args),*)
            .map(Into::<E>::into)
            .unwrap_or_else(|$($err_args),*| $self.next.$method($($args),*).into())
    };
}

macro_rules_method_with_optional_args!(impl_binary_expr_method {
    ($method: ident + $($arg: ident : $arg_type: ty),* $(,)?) => {
        fn $method<'a>(
            &mut self,
            operands: Self::ExprRefPair<'a>,
            $($arg: $arg_type),*
        ) -> Self::Expr<'a> {
            try_on_current_then_next!(self, $method, (operands$(, $arg)*), |operands|)
        }
    };
});

impl<C, N, E, CE> BinaryExprBuilder for ChainedExprBuilder<C, N, E, CE>
where
    N: BinaryExprBuilder,
    for<'a> C: BinaryExprBuilder<
            ExprRefPair<'a> = N::ExprRefPair<'a>,
            Expr<'a> = Result<CE, N::ExprRefPair<'a>>,
        >,
    CE: Into<E>,
    for<'a> N::Expr<'a>: Into<E>,
{
    type ExprRefPair<'a> = N::ExprRefPair<'a>;
    type Expr<'a> = E;

    fn binary_op<'a>(
        &mut self,
        operands: Self::ExprRefPair<'a>,
        op: BinaryOp,
        checked: bool,
    ) -> Self::Expr<'a> {
        try_on_current_then_next!(self, binary_op, (operands, op, checked), |operands|)
    }

    impl_binary_expr_method!(add sub mul + checked: bool);

    impl_binary_expr_method!(div rem);
    impl_binary_expr_method!(and or xor);
    impl_binary_expr_method!(shl shr);
    impl_binary_expr_method!(eq ne lt le gt ge);
    impl_binary_expr_method!(offset);
}

impl<C, N, E, CE> UnaryExprBuilder for ChainedExprBuilder<C, N, E, CE>
where
    N: UnaryExprBuilder,
    for<'a> C:
        UnaryExprBuilder<ExprRef<'a> = N::ExprRef<'a>, Expr<'a> = Result<CE, N::ExprRef<'a>>>,
    CE: Into<E>,
    for<'a> N::Expr<'a>: Into<E>,
{
    type ExprRef<'a> = N::ExprRef<'a>;
    type Expr<'a> = E;

    fn unary_op<'a>(&mut self, operand: Self::ExprRef<'a>, op: UnaryOp) -> Self::Expr<'a> {
        try_on_current_then_next!(self, unary_op, (operand, op), |operand|)
    }

    fn not<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
        try_on_current_then_next!(self, not, operand)
    }

    fn neg<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
        try_on_current_then_next!(self, neg, operand)
    }

    fn address_of<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
        try_on_current_then_next!(self, address_of, operand)
    }

    fn len<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
        try_on_current_then_next!(self, len, operand)
    }

    fn cast<'a>(&mut self, operand: Self::ExprRef<'a>, target: CastKind) -> Self::Expr<'a> {
        try_on_current_then_next!(self, cast, (operand, target.clone()), |operand|)
    }
}
