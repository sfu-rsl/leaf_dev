pub(crate) mod chained;
pub(crate) mod composite;
pub(crate) mod macros;
pub(crate) mod proj;
pub(crate) mod variance;

use super::{BinaryOp, CastKind, UnaryOp};

macro_rules! bin_fn_signature {
    ($method:ident $(, $arg: ident : $arg_type: ty)*) => {
        fn $method<'a>(&mut self, operands: Self::ExprRefPair<'a>, $($arg: $arg_type),*) -> Self::Expr<'a>;
    };
}

pub(crate) trait BinaryExprBuilder {
    type ExprRefPair<'a>;
    type Expr<'a>;

    fn binary_op<'a>(
        &mut self,
        operands: Self::ExprRefPair<'a>,
        op: BinaryOp,
        checked: bool,
    ) -> Self::Expr<'a>;

    bin_fn_signature!(add, checked: bool);
    bin_fn_signature!(sub, checked: bool);
    bin_fn_signature!(mul, checked: bool);

    bin_fn_signature!(div);
    bin_fn_signature!(rem);
    bin_fn_signature!(and);
    bin_fn_signature!(or);
    bin_fn_signature!(xor);
    bin_fn_signature!(shl);
    bin_fn_signature!(shr);
    bin_fn_signature!(eq);
    bin_fn_signature!(ne);
    bin_fn_signature!(lt);
    bin_fn_signature!(le);
    bin_fn_signature!(gt);
    bin_fn_signature!(ge);
    bin_fn_signature!(offset);
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
