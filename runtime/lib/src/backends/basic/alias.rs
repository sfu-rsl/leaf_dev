use super::{
    expr::{SymBinaryOperands, SymValueRef, ValueRef},
    LazyTypeInfo,
};
use crate::abs::{
    self,
    expr::{BinaryExprBuilder, CastExprBuilder, UnaryExprBuilder},
    FloatType, IntType, TypeId,
};
use common::tyexp::TypeInfo;

pub(crate) use crate::utils::alias::*;

pub(crate) trait SymValueRefExprBuilder
where
    Self: for<'a> BinaryExprBuilder<ExprRefPair<'a> = SymBinaryOperands, Expr<'a> = ValueRef>
        + for<'a> UnaryExprBuilder<ExprRef<'a> = SymValueRef, Expr<'a> = ValueRef>
        + for<'a> CastExprBuilder<
            ExprRef<'a> = SymValueRef,
            Expr<'a> = ValueRef,
            Metadata<'a> = LazyTypeInfo,
            IntType = IntType,
            FloatType = FloatType,
            PtrType = TypeId,
            GenericType = TypeId,
        >,
{
}

pub(crate) trait ValueRefExprBuilder
where
    Self: for<'a> BinaryExprBuilder<ExprRefPair<'a> = (ValueRef, ValueRef), Expr<'a> = ValueRef>
        + for<'a> UnaryExprBuilder<ExprRef<'a> = ValueRef, Expr<'a> = ValueRef>
        + for<'a> CastExprBuilder<
            ExprRef<'a> = ValueRef,
            Expr<'a> = ValueRef,
            Metadata<'a> = LazyTypeInfo,
            IntType = IntType,
            FloatType = FloatType,
            PtrType = TypeId,
            GenericType = TypeId,
        >,
{
}

pub(crate) trait ValueRefBinaryExprBuilder
where
    Self: for<'a> BinaryExprBuilder<ExprRefPair<'a> = (ValueRef, ValueRef), Expr<'a> = ValueRef>,
{
}

pub(crate) use super::expr::builders::DefaultExprBuilder as BasicExprBuilder;
pub(crate) use super::expr::builders::DefaultSymExprBuilder as BasicSymExprBuilder;

pub(crate) trait TypeManager:
    abs::backend::TypeManager<Key = abs::TypeId, Value = &'static TypeInfo>
{
}
impl<'t, T> TypeManager for T where
    T: abs::backend::TypeManager<Key = abs::TypeId, Value = &'static TypeInfo>
{
}
