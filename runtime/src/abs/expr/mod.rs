pub(crate) mod chained;
pub(crate) mod composite;
pub(crate) mod macros;
pub(crate) mod proj;
pub(crate) mod variance;

use super::{BinaryOp, CastKind, UnaryOp};
use macros::{for_all_binary_op, repeat_macro_for};

macro_rules! bin_fn_signature {
    ($($name:ident),*) => {
        $(
            fn $name<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a>;
        )*
    };
}

pub(crate) trait BinaryExprBuilder {
    type ExprRefPair<'a>;
    type Expr<'a>;

    fn binary_op<'a>(&mut self, operands: Self::ExprRefPair<'a>, op: BinaryOp) -> Self::Expr<'a>;

    for_all_binary_op!(bin_fn_signature);
}

pub(crate) trait UnaryExprBuilder {
    type ExprRef<'a>;
    type Expr<'a> = Self::ExprRef<'a>;

    fn unary_op<'a>(&mut self, operand: Self::ExprRef<'a>, op: UnaryOp) -> Self::Expr<'a>;

    fn not<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a>;

    fn neg<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a>;

    fn address_of<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a>;

    fn len<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a>;

    fn cast<'a>(&mut self, operand: Self::ExprRef<'a>, target: CastKind) -> Self::Expr<'a>;
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
