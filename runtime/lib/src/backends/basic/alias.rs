use super::{expr::ValueRef, LazyTypeInfo};
use crate::abs::{
    self,
    expr::{BinaryExprBuilder, CastExprBuilder, UnaryExprBuilder},
    FloatType, IntType, TypeId,
};
use common::tyexp::TypeInfo;

pub(crate) use crate::utils::alias::*;

pub(crate) trait ValueRefExprBuilder
where
    Self: for<'a> BinaryExprBuilder<
            ExprRefPair<'a> = <Self as EB>::ExprRefPair<'a>,
            Expr<'a> = <Self as EB>::Expr<'a>,
        > + for<'a> UnaryExprBuilder<
            ExprRef<'a> = <Self as EB>::ExprRef<'a>,
            Expr<'a> = <Self as EB>::Expr<'a>,
        > + for<'a> CastExprBuilder<
            ExprRef<'a> = <Self as EB>::ExprRef<'a>,
            Expr<'a> = <Self as EB>::Expr<'a>,
            Metadata<'a> = LazyTypeInfo,
            IntType = IntType,
            FloatType = FloatType,
            PtrType = TypeId,
            GenericType = TypeId,
        >,
{
    type ExprRef<'a>: From<ValueRef>;
    type ExprRefPair<'a>: From<(ValueRef, ValueRef)>;
    type Expr<'a>: Into<ValueRef>;
}

use ValueRefExprBuilder as EB;

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
