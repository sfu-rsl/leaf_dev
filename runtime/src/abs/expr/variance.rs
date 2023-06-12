/// This module provides some traits to make it easier to implement expression builders for wrappers
/// that work as adapters. Most of the adapters provide non-trivial covariance and contravariance
/// over the input and output types of the wrapped expression builder.
use super::{macros::*, BinaryExprBuilder, UnaryExprBuilder};
use crate::abs::{BinaryOp, FloatType, IntType, UnaryOp};
use std::ops::DerefMut;

use BinaryExprBuilder as BEB;
use UnaryExprBuilder as UEB;

pub(crate) trait FnH<I, O>: FnOnce(I) -> O {}
impl<I, O, F> FnH<I, O> for F where F: FnOnce(I) -> O {}

pub(crate) trait BinaryExprBuilderAdapter: DerefMut
where
    Self::Target: Sized,
    Self::Target: BinaryExprBuilder,
{
    type TargetExprRefPair<'a> = <Self::Target as BEB>::ExprRefPair<'a>;
    type TargetExpr<'a> = <Self::Target as BEB>::Expr<'a>;

    /// Takes the input from the type of the adapted operands, passes it to the wrapped builder,
    /// then converts and returns the output to the type of the adapted expression.
    /// [`build`] is the method in the wrapped builder that corresponds to the one called currently
    /// on this builder.
    fn adapt<'t, F>(operands: Self::TargetExprRefPair<'t>, build: F) -> Self::TargetExpr<'t>
    where
        F: for<'s> FnH<<Self::Target as BEB>::ExprRefPair<'s>, <Self::Target as BEB>::Expr<'s>>;
}

macro_rules! delegate_singular_binary_op {
    ($method: ident $(, $arg: ident : $arg_type: ty)*) => {
        fn $method<'a>(
            &mut self,
            operands: Self::ExprRefPair<'a>,
            $($arg: $arg_type),*
        ) -> Self::Expr<'a> {
            Self::adapt(operands, |operands| self.deref_mut().$method(operands, $($arg),*))
        }
    };
}

impl<T: BinaryExprBuilderAdapter> BinaryExprBuilder for T
where
    T::Target: Sized,
    T::Target: BinaryExprBuilder,
{
    type ExprRefPair<'a> = T::TargetExprRefPair<'a>;
    type Expr<'a> = T::TargetExpr<'a>;

    delegate_singular_binary_op!(binary_op, op: BinaryOp);
    for_all_binary_op!(delegate_singular_binary_op);
}

pub(crate) trait UnaryExprBuilderAdapter: DerefMut
where
    Self::Target: Sized,
    Self::Target: UnaryExprBuilder,
{
    type TargetExprRef<'a> = <Self::Target as UEB>::ExprRef<'a>;
    type TargetExpr<'a> = <Self::Target as UEB>::Expr<'a>;

    /// Takes the input from the type of the adapted operand, passes it to the wrapped builder,
    /// then converts and returns the output to the type of the adapted expression.
    /// [`build`] is the method in the wrapped builder that corresponds to the one called currently
    /// on this builder.
    fn adapt<'t, F>(operand: Self::TargetExprRef<'t>, build: F) -> Self::TargetExpr<'t>
    where
        F: for<'s> FnH<<Self::Target as UEB>::ExprRef<'s>, <Self::Target as UEB>::Expr<'s>>;
}

macro_rules! delegate_singular_unary_op {
    ($method: ident $(, $arg: ident : $arg_type: ty)*) => {
        fn $method<'a>(
            &mut self,
            operand: Self::ExprRef<'a>,
            $($arg: $arg_type),*
        ) -> Self::Expr<'a> {
            Self::adapt(operand, |operand| self.deref_mut().$method(operand, $($arg),*))
        }
    };
}

impl<T: UnaryExprBuilderAdapter> UnaryExprBuilder for T
where
    T::Target: Sized,
    T::Target: UnaryExprBuilder,
{
    type ExprRef<'a> = T::TargetExprRef<'a>;
    type Expr<'a> = T::TargetExpr<'a>;

    delegate_singular_unary_op!(unary_op, op: UnaryOp);
    delegate_singular_unary_op!(not);
    delegate_singular_unary_op!(neg);
    delegate_singular_unary_op!(address_of);
    delegate_singular_unary_op!(len);
    delegate_singular_unary_op!(cast_to_char);
    delegate_singular_unary_op!(cast_to_int, to: IntType);
    delegate_singular_unary_op!(cast_to_float, to: FloatType);
}
