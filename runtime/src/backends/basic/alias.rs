use super::expr::{ProjExpr, SymIndexPair, SymValueRef, ValueRef};
use crate::abs::expr::{proj::Projector, BinaryExprBuilder, ExprBuilder};

pub(crate) trait ValueRefExprBuilder: ExprBuilder<ValueRef> {}
impl<T> ValueRefExprBuilder for T where T: ExprBuilder<ValueRef> {}

/* NOTE: Because of an internal compiler bug, the blanket impl can't be added
 * for these alias traits. See: https://github.com/rust-lang/rust/issues/112097
 */
use ValueRefBinaryExprBuilder as VBEB;
pub(crate) trait ValueRefBinaryExprBuilder
where
    Self: for<'a> BinaryExprBuilder<
            ExprRefPair<'a> = <Self as VBEB>::ExprRefPair<'a>,
            Expr<'a> = <Self as VBEB>::Expr<'a>,
        >,
{
    type ExprRefPair<'a>: From<(ValueRef, ValueRef)> = (ValueRef, ValueRef);
    type Expr<'a>: Into<ValueRef> = ValueRef;
}

use SymValueRefProjector as SVP;
pub(crate) trait SymValueRefProjector
where
    Self: for<'a> Projector<
            HostRef<'a> = <Self as SVP>::HostRef<'a>,
            HIRefPair<'a> = <Self as SVP>::HIRefPair<'a>,
            Proj<'a> = <Self as SVP>::Proj<'a>,
        >,
{
    type HostRef<'a>: From<SymValueRef> = SymValueRef;
    type HIRefPair<'a>: From<SymIndexPair> = SymIndexPair;
    type Proj<'a>: Into<ProjExpr> = ProjExpr;
}