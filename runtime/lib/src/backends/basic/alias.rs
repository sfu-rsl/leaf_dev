use super::expr::ValueRef;
use crate::abs::{
    self,
    expr::{BinaryExprBuilder, ExprBuilder},
};
use common::tyexp::TypeInfo;

pub(crate) use crate::utils::alias::*;

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

pub(crate) trait TypeManager:
    abs::backend::TypeManager<Key = abs::TypeId, Value = &'static TypeInfo>
{
}
impl<'t, T> TypeManager for T where
    T: abs::backend::TypeManager<Key = abs::TypeId, Value = &'static TypeInfo>
{
}
