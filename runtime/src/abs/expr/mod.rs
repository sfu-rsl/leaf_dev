pub(crate) mod chained;
pub(crate) mod composite;
pub(crate) mod macros;
pub(crate) mod proj;
pub(crate) mod variance;

use super::{BinaryOp, CastKind, UnaryOp};

macro_rules! bin_fn_signature {
    ($($method:ident)*) => { 
        $(bin_fn_signature!($method +);)* 
    };
    ($($method:ident)* + $arg: ident : $arg_type: ty) => { 
        $(bin_fn_signature!($method + $arg: $arg_type,);)* 
    };
    ($method: ident + $($arg: ident : $arg_type: ty),* $(,)?) => {
        fn $method<'a>(
            &mut self, 
            operands: Self::ExprRefPair<'a>, 
            $($arg: $arg_type),*
        ) -> Self::Expr<'a>;
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

    bin_fn_signature!(add sub mul + checked: bool);

    bin_fn_signature!(div rem);
    bin_fn_signature!(and or xor);
    bin_fn_signature!(shl shr);
    bin_fn_signature!(eq ne lt le gt ge);
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
