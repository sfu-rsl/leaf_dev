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

macro_rules_method_with_optional_args!(impl_binary_expr_method {
    ($method: ident + $($arg: ident : $arg_type: ty),* $(,)?) => {
        delegate! {
            to self.binary {
                fn $method<'a>(
                    &mut self,
                    operands: Self::ExprRefPair<'a>,
                    $($arg: $arg_type),*
                ) -> Self::Expr<'a>;
            }
        }
    };
});

macro_rules_method_with_optional_args!(impl_unary_expr_method {
    ($method: ident + $($arg: ident : $arg_type: ty),* $(,)?) => {
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
});

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

impl<B, U> UnaryExprBuilder for CompositeExprBuilder<B, U>
where
    B: BinaryExprBuilder,
    U: UnaryExprBuilder,
{
    type ExprRef<'a> = U::ExprRef<'a>;
    type Expr<'a> = U::Expr<'a>;

    impl_unary_expr_method!(unary_op + op: UnaryOp);

    impl_unary_expr_method!(not neg ptr_metadata);
    impl_unary_expr_method!(address_of len discriminant);
    impl_unary_expr_method!(cast + target: CastKind);
}
