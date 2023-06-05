use delegate::delegate;

use crate::abs::{BinaryOp, UnaryOp};

use super::*;

// This is the main expression builder, which is simply an interface for the binary & unary expression builders.
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
                    operands: <Self as BinaryExprBuilder>::ExprRefPair<'a>,
                ) -> <Self as BinaryExprBuilder>::Expr<'a>;
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

    delegate! {
        to self.unary {
            fn unary_op<'a>(&mut self, operand: Self::ExprRef<'a>, op: UnaryOp) -> Self::Expr<'a>;

            fn not<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a>;

            fn neg<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a>;

            fn address_of<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a>;

            fn len<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a>;

            fn cast_to_char<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a>;

            fn cast_to_int<'a>(
                &mut self,
                operand: Self::ExprRef<'a>,
                to_bits: u64,
                to_signed: bool,
            ) -> Self::Expr<'a>;

            fn cast_to_float<'a>(
                &mut self,
                operand: Self::ExprRef<'a>,
                to_bits: u64,
            ) -> Self::Expr<'a>;
        }
    }
}
