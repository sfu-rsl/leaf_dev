use std::marker::PhantomData;

use super::*;

/// A builder that first tries to build an expression using the current builder,
/// and if it fails (does not support building it), it tries to build it using
/// the parent builder.
/// The current builder should return a `Result` that either is the built
/// expression (as `Ok`) or the expressions passed to it (as `Err`).
pub(crate) struct ChainedExprBuilder<Current, Next, Expr, CurrentExpr: Into<Expr> = Expr> {
    pub(crate) current: Current,
    pub(crate) next: Next,
    _phantom: PhantomData<(Expr, CurrentExpr)>,
}

impl<C, N, E, EC> Default for ChainedExprBuilder<C, N, E, EC>
where
    C: Default,
    N: Default,
    EC: Into<E>,
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
        $self.current
            .$method($arg)
            .map(Into::<E>::into)
            .unwrap_or_else(|$arg| $self.next.$method($arg).into())
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

macro_rules! impl_binary_expr_method {
    ($method:ident) => {
        fn $method<'a>(
            &mut self,
            operands: <Self as BinaryExprBuilder>::ExprRefPair<'a>,
        ) -> <Self as BinaryExprBuilder>::Expr<'a> {
            try_on_current_then_next!(self, $method, operands)
        }
    };
}

impl<C, N, E, EC> BinaryExprBuilder for ChainedExprBuilder<C, N, E, EC>
where
    N: BinaryExprBuilder,
    for<'a> C: BinaryExprBuilder<
            ExprRefPair<'a> = N::ExprRefPair<'a>,
            Expr<'a> = Result<EC, N::ExprRefPair<'a>>,
        >,
    EC: Into<E>,
    for<'a> N::Expr<'a>: Into<E>,
{
    type ExprRefPair<'a> = N::ExprRefPair<'a>;
    type Expr<'a> = E;

    fn binary_op<'a>(&mut self, operands: Self::ExprRefPair<'a>, op: BinaryOp) -> Self::Expr<'a> {
        try_on_current_then_next!(self, binary_op, (operands, op), |operands|)
    }

    for_all_binary_op!(impl_binary_expr_method);
}

impl<C, N, E, EC> UnaryExprBuilder for ChainedExprBuilder<C, N, E, EC>
where
    N: UnaryExprBuilder,
    for<'a> C:
        UnaryExprBuilder<ExprRef<'a> = N::ExprRef<'a>, Expr<'a> = Result<EC, N::ExprRef<'a>>>,
    EC: Into<E>,
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

    fn cast_to_char<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
        try_on_current_then_next!(self, cast_to_char, operand)
    }

    fn cast_to_int<'a>(
        &mut self,
        operand: Self::ExprRef<'a>,
        to_bits: u64,
        to_signed: bool,
    ) -> Self::Expr<'a> {
        try_on_current_then_next!(self, cast_to_int, (operand, to_bits, to_signed), |operand|)
    }

    fn cast_to_float<'a>(&mut self, operand: Self::ExprRef<'a>, to_bits: u64) -> Self::Expr<'a> {
        try_on_current_then_next!(self, cast_to_float, (operand, to_bits), |operand|)
    }
}
