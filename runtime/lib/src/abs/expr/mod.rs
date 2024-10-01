pub(crate) mod chained;
pub(crate) mod composite;
pub(crate) mod macros;
pub(crate) mod proj;
pub(crate) mod sym_place;
pub(crate) mod variance;

use self::macros::macro_rules_method_with_optional_args;

use super::{BinaryOp, CastKind, UnaryOp};

macro_rules_method_with_optional_args! (bin_fn_signature {
    ($method: ident + $($arg: ident : $arg_type: ty),* $(,)?) => {
        fn $method<'a>(
            &mut self,
            operands: Self::ExprRefPair<'a>,
            $($arg: $arg_type),*
        ) -> Self::Expr<'a>;
    };
});

macro_rules_method_with_optional_args! (unary_fn_signature {
    ($method: ident + $($arg: ident : $arg_type: ty),* $(,)?) => {
        fn $method<'a>(
            &mut self,
            operand: Self::ExprRef<'a>,
            $($arg: $arg_type),*
        ) -> Self::Expr<'a>;
    };
});

pub(crate) trait BinaryExprBuilder {
    type ExprRefPair<'a>;
    type Expr<'a>;

    fn binary_op<'a>(&mut self, operands: Self::ExprRefPair<'a>, op: BinaryOp) -> Self::Expr<'a>;

    bin_fn_signature!(add add_unchecked add_with_overflow add_saturating);
    bin_fn_signature!(sub sub_unchecked sub_with_overflow sub_saturating);
    bin_fn_signature!(mul mul_unchecked mul_with_overflow);
    bin_fn_signature!(div rem div_exact);
    bin_fn_signature!(and or xor);
    bin_fn_signature!(shl shl_unchecked shr shr_unchecked);
    bin_fn_signature!(rotate_left rotate_right);
    bin_fn_signature!(eq ne lt le gt ge cmp);
    bin_fn_signature!(offset);
}

pub(crate) trait UnaryExprBuilder {
    type ExprRef<'a>;
    type Expr<'a> = Self::ExprRef<'a>;

    fn unary_op<'a>(&mut self, operand: Self::ExprRef<'a>, op: UnaryOp) -> Self::Expr<'a>;

    unary_fn_signature!(not neg ptr_metadata);
    unary_fn_signature!(address_of len discriminant);

    unary_fn_signature!(cast + target: CastKind);
}

// NOTE: Because of an internal compiler bug, the blanket impl can't be added.

use ExprBuilder as EB;
pub(crate) trait ExprBuilder<R, E = R>
where
    Self: for<'a> BinaryExprBuilder<
            ExprRefPair<'a> = <Self as EB<R, E>>::ExprRefPair<'a>,
            Expr<'a> = <Self as EB<R, E>>::Expr<'a>,
        > + for<'a> UnaryExprBuilder<
            ExprRef<'a> = <Self as EB<R, E>>::ExprRef<'a>,
            Expr<'a> = <Self as EB<R, E>>::Expr<'a>,
        >,
{
    type ExprRef<'a>: From<R>;
    type ExprRefPair<'a>: From<(R, R)>;
    type Expr<'a>: Into<E>;
}

pub(crate) use {chained::ChainedExprBuilder, composite::CompositeExprBuilder};
