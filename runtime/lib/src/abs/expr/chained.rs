use std::marker::PhantomData;

use super::*;

/// A builder that first tries to build an expression using the current builder,
/// and if it fails (does not support building it), it tries to build it using
/// the next builder.
///
/// The current builder should return a `Result` that either is the built
/// expression (as `Ok`) or the expressions passed to it (as `Err`).
/// The builder may be a transformer over the expressions by returning an `Err`
/// containing the transformed version.
///
/// This struct is useful for overriding certain functions with optimizations, for example.
pub(crate) struct ChainedExprBuilder<Current, Next, Expr, CurrentExpr: Into<Expr> = Expr> {
    pub(crate) current: Current,
    pub(crate) next: Next,
    _phantom: PhantomData<(Expr, CurrentExpr)>,
}

impl<C, N, E, CE: Into<E>> ChainedExprBuilder<C, N, E, CE> {
    pub(crate) fn new(current: C, next: N) -> Self {
        Self {
            current,
            next,
            _phantom: Default::default(),
        }
    }
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
        impl_binary_expr_method!($method + ($($arg : $arg_type { $arg }),*));
    };
    /* Gives the ability to give a custom expression passed to the builders. */
    ($method: ident + ($($arg: ident : $arg_type: ty { $arg_expr:expr }),*) $(,)?) => {
        fn $method<'a>(
            &mut self,
            operands: Self::ExprRefPair<'a>,
            $($arg: $arg_type),*
        ) -> Self::Expr<'a> {
            try_on_current_then_next!(self, $method, (operands$(, $arg_expr)*), |operands|)
        }
    };
});

macro_rules_method_with_optional_args!(impl_unary_expr_method {
    ($method: ident + $($arg: ident : $arg_type: ty),* $(,)?) => {
        impl_unary_expr_method!($method + ($($arg : $arg_type { $arg }),*));
    };
    /* Gives the ability to give a custom expression passed to the builders. */
    ($method: ident + ($($arg: ident : $arg_type: ty { $arg_expr:expr }),*) $(,)?) => {
        fn $method<'a>(
            &mut self,
            operand: Self::ExprRef<'a>,
            $($arg: $arg_type),*
        ) -> Self::Expr<'a> {
            try_on_current_then_next!(self, $method, (operand$(, $arg_expr)*), |operand|)
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

    fn binary_op<'a>(&mut self, operands: Self::ExprRefPair<'a>, op: BinaryOp) -> Self::Expr<'a> {
        try_on_current_then_next!(self, binary_op, (operands, op), |operands|)
    }

    impl_binary_expr_method!(add add_unchecked add_with_overflow);
    impl_binary_expr_method!(sub sub_unchecked sub_with_overflow);
    impl_binary_expr_method!(mul mul_unchecked mul_with_overflow);
    impl_binary_expr_method!(div rem);
    impl_binary_expr_method!(and or xor);
    impl_binary_expr_method!(shl shl_unchecked);
    impl_binary_expr_method!(shr shr_unchecked);
    impl_binary_expr_method!(eq ne lt le gt ge cmp);
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

    impl_unary_expr_method!(unary_op + op: UnaryOp);

    impl_unary_expr_method!(not neg ptr_metadata);
    impl_unary_expr_method!(address_of len discriminant);
    impl_unary_expr_method!(cast + target: CastKind);
}
