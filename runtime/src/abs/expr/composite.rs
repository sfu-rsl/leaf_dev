use delegate::delegate;

use crate::abs::{BinaryOp, UnaryOp};

use super::*;

/// This is the main expression builder, which is simply an interface for the
/// binary & unary expression builders.
#[derive(Default)]
pub(crate) struct CompositeExprBuilder<B: BinaryExprBuilder, U: UnaryExprBuilder> {
    pub(crate) binary: B,
    pub(crate) unary: U,
}

macro_rules! impl_binary_expr_method {
    ($method:ident) => {
        delegate! {
            to self.binary {
                fn $method<'a>(
                    &mut self,
                    operands: Self::ExprRefPair<'a>,
                ) -> Self::Expr<'a>;
            }
        }
    };
}

macro_rules! impl_unary_expr_method {
    ($method: ident $(, $arg: ident : $arg_type: ty)*) => {
        delegate! {
            to self.unary {
                fn $method<'a>(
                    &mut self,
                    operand: Self::ExprRef<'a>,
                    $($arg: $arg_type),*
                ) -> Self::Expr<'a>;
            }
        }
    };
}

impl<B, U> BinaryExprBuilder for CompositeExprBuilder<B, U>
where
    B: BinaryExprBuilder,
    U: UnaryExprBuilder,
{
    type ExprRefPair<'a> = B::ExprRefPair<'a>;
    type Expr<'a> = B::Expr<'a>;

    delegate! {
        to self.binary {
            fn binary_op<'a>(
                &mut self,
                operands: Self::ExprRefPair<'a>,
                op: BinaryOp,
            ) -> Self::Expr<'a>;
        }
    }

    for_all_binary_op!(impl_binary_expr_method);
}

impl<B, U> UnaryExprBuilder for CompositeExprBuilder<B, U>
where
    B: BinaryExprBuilder,
    U: UnaryExprBuilder,
{
    type ExprRef<'a> = U::ExprRef<'a>;
    type Expr<'a> = U::Expr<'a>;

    impl_unary_expr_method!(unary_op, op: UnaryOp);

    impl_unary_expr_method!(not);
    impl_unary_expr_method!(neg);
    impl_unary_expr_method!(address_of);
    impl_unary_expr_method!(len);
    impl_unary_expr_method!(cast_to_char);

    impl_unary_expr_method!(cast_to_int, to_bits: u64, to_signed: bool);
    impl_unary_expr_method!(cast_to_float, to_bits: u64);
}
