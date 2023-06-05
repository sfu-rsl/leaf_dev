use super::super::alias::ValueRefBinaryExprBuilder;
use super::*;
use crate::abs::{
    expr::{
        macros::*, BinaryExprBuilder, ChainedExprBuilder, CompositeExprBuilder, ExprBuilder,
        UnaryExprBuilder,
    },
    BinaryOp, UnaryOp,
};

type Composite<Binary, Unary> = CompositeExprBuilder<Binary, Unary>;
type Chained<Current, Next, Expr = ValueRef> = ChainedExprBuilder<Current, Next, Expr>;

pub(crate) type DefaultExprBuilder = toplevel::TopLevelBuilder;

pub(crate) fn new_expr_builder() -> DefaultExprBuilder {
    DefaultExprBuilder::default()
}

impl ExprBuilder<ValueRef> for DefaultExprBuilder {
    type ExprRef<'a> = ValueRef;
    type ExprRefPair<'a> = (ValueRef, ValueRef);
    type Expr<'a> = ValueRef;
}

impl ValueRefBinaryExprBuilder for DefaultExprBuilder {}

mod toplevel {

    use super::{adapters::ConcreteBuilder, symbolic::SymbolicBuilder, *};

    /// An expression builder that separates the path for expressions that involve symbolic values,
    /// or the ones that are fully based on concrete values.
    /// NOTE: In an ideal case, fully concrete expressions should not be asked to created. So in
    /// the future, this top-level builder will be reduced to the symbolic builder.
    #[derive(Default)]
    pub(crate) struct TopLevelBuilder {
        sym_builder: SymbolicBuilder,
        conc_builder: ConcreteBuilder,
    }

    impl BinaryExprBuilder for TopLevelBuilder {
        type ExprRefPair<'a> = (ValueRef, ValueRef);
        type Expr<'a> = ValueRef;

        fn binary_op<'a>(
            &mut self,
            (first, second): Self::ExprRefPair<'a>,
            op: BinaryOp,
        ) -> Self::Expr<'a> {
            // TODO: Add support for checked operations.

            if first.is_symbolic() {
                self.sym_builder.binary_op(
                    BinaryOperands::Orig {
                        first: SymValueRef::new(first),
                        second,
                    },
                    op,
                )
            } else if second.is_symbolic() {
                self.sym_builder.binary_op(
                    BinaryOperands::Rev {
                        first: second,
                        second: SymValueRef::new(first),
                    },
                    op,
                )
            } else {
                self.conc_builder.binary_op(
                    (ConcreteValueRef::new(first), ConcreteValueRef::new(second)),
                    op,
                )
            }
        }

        impl_singular_binary_ops_through_general!();
    }

    macro_rules! call_unary_method {
        ($self:ident, $method:ident, $operand:expr $(,$args:expr)* $(,)?) => {
            if $operand.is_symbolic() {
                $self.sym_builder
                    .$method(SymValueRef::new($operand).into(), $($args),*)
                    .into()
            } else {
                $self.conc_builder
                    .$method(ConcreteValueRef::new($operand).into(), $($args),*)
            }
        };
    }

    impl UnaryExprBuilder for TopLevelBuilder {
        type ExprRef<'a> = ValueRef;
        type Expr<'a> = ValueRef;

        fn unary_op<'a>(&mut self, operand: Self::ExprRef<'a>, op: UnaryOp) -> Self::Expr<'a> {
            call_unary_method!(self, unary_op, operand, op)
        }

        fn not<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            call_unary_method!(self, not, operand)
        }

        fn neg<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            call_unary_method!(self, neg, operand)
        }

        fn address_of<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            call_unary_method!(self, address_of, operand)
        }

        fn len<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            call_unary_method!(self, len, operand)
        }

        fn cast_to_char<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            call_unary_method!(self, cast_to_char, operand)
        }

        fn cast_to_int<'a>(
            &mut self,
            operand: Self::ExprRef<'a>,
            to_bits: u64,
            to_signed: bool,
        ) -> Self::Expr<'a> {
            call_unary_method!(self, cast_to_int, operand, to_bits, to_signed)
        }

        fn cast_to_float<'a>(
            &mut self,
            operand: Self::ExprRef<'a>,
            to_bits: u64,
        ) -> Self::Expr<'a> {
            call_unary_method!(self, cast_to_float, operand, to_bits)
        }
    }
}

mod symbolic {
    use super::{adapters::ConstSimplifier, core::CoreBuilder, *};

    pub(crate) type SymbolicBuilder = Composite<
        /*Binary:*/ Chained<ConstSimplifier, CoreBuilder>,
        /*Unary:*/ CoreBuilder,
    >;
}

mod adapters {
    use deref_derive::{Deref, DerefMut};

    use crate::abs::expr::variance::*;

    use super::*;

    use BinaryExprBuilder as BEB;
    use UnaryExprBuilder as UEB;

    impl From<Expr> for ValueRef {
        fn from(expr: Expr) -> Self {
            expr.into()
        }
    }

    #[derive(Default, Clone, Deref, DerefMut)]
    pub(crate) struct ConcreteBuilder(concrete::ConcreteBuilder);

    impl BinaryExprBuilderAdapter for ConcreteBuilder {
        type TargetExprRefPair<'a> = (ConcreteValueRef, ConcreteValueRef);
        type TargetExpr<'a> = ValueRef;

        fn adapt<'t, F>(operands: Self::TargetExprRefPair<'t>, build: F) -> Self::TargetExpr<'t>
        where
            F: for<'s> FnH<<Self::Target as BEB>::ExprRefPair<'s>, <Self::Target as BEB>::Expr<'s>>,
        {
            ValueRef::new(build((operands.0.as_ref(), operands.1.as_ref())).into())
        }
    }

    impl UnaryExprBuilderAdapter for ConcreteBuilder {
        type TargetExprRef<'a> = ConcreteValueRef;
        type TargetExpr<'a> = ValueRef;

        fn adapt<'t, F>(operand: Self::TargetExprRef<'t>, build: F) -> Self::TargetExpr<'t>
        where
            F: for<'s> FnH<<Self::Target as UEB>::ExprRef<'s>, <Self::Target as UEB>::Expr<'s>>,
        {
            ValueRef::new(build(operand.as_ref()).into())
        }
    }

    #[derive(Default, Clone, Deref, DerefMut)]
    pub(crate) struct ConstSimplifier(simp::ConstSimplifier<SymValueRef, ValueRef>);

    impl BinaryExprBuilderAdapter for ConstSimplifier {
        type TargetExprRefPair<'a> = SymBinaryOperands;
        type TargetExpr<'a> = Result<ValueRef, SymBinaryOperands>;

        fn adapt<'t, F>(operands: Self::TargetExprRefPair<'t>, build: F) -> Self::TargetExpr<'t>
        where
            F: for<'s> FnH<<Self::Target as BEB>::ExprRefPair<'s>, <Self::Target as BEB>::Expr<'s>>,
        {
            let (first, second, is_reversed) = operands.flatten();
            let first = match second.as_ref() {
                Value::Concrete(ConcreteValue::Const(value)) => {
                    match build((first, value, is_reversed).into()) {
                        Ok(result) => Ok(result),
                        Err(operands) => Err(operands.flatten().0),
                    }
                }
                _ => Err(first),
            };

            first.map_err(|first| (first, second, is_reversed).into())
        }
    }
}

mod core {
    use super::*;

    #[derive(Default, Clone)]
    pub(crate) struct CoreBuilder;

    impl BinaryExprBuilder for CoreBuilder {
        type ExprRefPair<'a> = SymBinaryOperands;
        type Expr<'a> = Expr;

        fn binary_op<'a>(
            &mut self,
            operands: Self::ExprRefPair<'a>,
            op: BinaryOp,
        ) -> Self::Expr<'a> {
            Expr::Binary {
                operator: op,
                operands,
            }
        }

        impl_singular_binary_ops_through_general!();
    }

    impl UnaryExprBuilder for CoreBuilder {
        type ExprRef<'a> = SymValueRef;
        type Expr<'a> = Expr;

        fn unary_op<'a>(&mut self, operand: Self::ExprRef<'a>, op: UnaryOp) -> Self::Expr<'a> {
            Expr::Unary {
                operator: op,
                operand,
            }
        }

        impl_singular_unary_ops_through_general!();

        fn address_of<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            todo!("Add support for address of operator {:?}", operand)
        }

        fn len<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            Expr::Len { of: operand }
        }

        fn cast_to_char<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            Expr::Cast {
                from: operand,
                to: SymbolicVarType::Char,
            }
        }

        fn cast_to_int<'a>(
            &mut self,
            operand: Self::ExprRef<'a>,
            to_bits: u64,
            to_signed: bool,
        ) -> Self::Expr<'a> {
            Expr::Cast {
                from: operand,
                to: SymbolicVarType::Int {
                    size: to_bits,
                    is_signed: to_signed,
                },
            }
        }

        fn cast_to_float<'a>(
            &mut self,
            operand: Self::ExprRef<'a>,
            to_bits: u64,
        ) -> Self::Expr<'a> {
            todo!("Add support for float casts. {:?} {}", operand, to_bits)
        }
    }
}

mod concrete {
    use super::*;

    #[derive(Default, Clone)]
    pub(crate) struct ConcreteBuilder;

    impl BinaryExprBuilder for ConcreteBuilder {
        type ExprRefPair<'a> = (&'a ConcreteValue, &'a ConcreteValue);
        type Expr<'a> = ConcreteValue;

        fn binary_op<'a>(
            &mut self,
            (first, second): Self::ExprRefPair<'a>,
            op: BinaryOp,
        ) -> Self::Expr<'a> {
            match (first, second) {
                (ConcreteValue::Const(first_c), ConcreteValue::Const(second_c)) => {
                    ConstValue::binary_op(first_c, second_c, op).into()
                }
                _ => unreachable!(
                    "Binary operations for concrete values are only supposed to be called on constants."
                ),
            }
        }

        impl_singular_binary_ops_through_general!();
    }

    impl UnaryExprBuilder for ConcreteBuilder {
        type ExprRef<'a> = &'a ConcreteValue;
        type Expr<'a> = ConcreteValue;

        fn unary_op<'a>(&mut self, operand: Self::ExprRef<'a>, op: UnaryOp) -> Self::Expr<'a> {
            match operand {
                ConcreteValue::Const(c) => ConstValue::unary_op(c, op).into(),
                _ => unreachable!(
                    "Unary operations for concrete values are only supposed to be called on constants."
                ),
            }
        }

        impl_singular_unary_ops_through_general!();

        fn address_of<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            todo!("Add support for address of operator {:?}", operand)
        }

        fn len<'a>(&mut self, of: Self::ExprRef<'a>) -> Self::Expr<'a> {
            match of {
                ConcreteValue::Array(arr) => {
                    let len = arr.len();
                    ConstValue::Int {
                        bit_rep: len as u128,
                        size: std::mem::size_of_val(&len) as u64 * 8,
                        is_signed: false,
                    }
                    .into()
                }
                _ => unreachable!(
                    "Length operation for concrete values is supposed to be called on arrays."
                ),
            }
        }

        fn cast_to_char<'a>(&mut self, expr: Self::ExprRef<'a>) -> Self::Expr<'a> {
            match expr {
                ConcreteValue::Const(ConstValue::Int { bit_rep, .. }) => {
                    ConstValue::Char(*bit_rep as u8 as char).into()
                }
                _ => unreachable!("Char cast is supposed to be called on an integer."),
            }
        }

        fn cast_to_int<'a>(
            &mut self,
            expr: Self::ExprRef<'a>,
            to_bits: u64,
            to_signed: bool,
        ) -> Self::Expr<'a> {
            match expr {
                ConcreteValue::Const(c) => ConstValue::integer_cast(c, to_bits, to_signed).into(),
                _ => {
                    unreachable!("Integer cast is supposed to be called on a constant.")
                }
            }
        }

        fn cast_to_float<'a>(&mut self, expr: Self::ExprRef<'a>, to_bits: u64) -> Self::Expr<'a> {
            todo!("Add support for float casts. {:?} {}", expr, to_bits)
        }
    }
}

mod simp {
    use super::*;
    use std::marker::PhantomData;

    #[derive(Clone)]
    pub(crate) struct ConstSimplifier<R, E> {
        _phantom: PhantomData<(R, E)>,
    }

    impl<R, E> Default for ConstSimplifier<R, E> {
        fn default() -> Self {
            Self {
                _phantom: PhantomData,
            }
        }
    }

    type WithConstOperand<'a, T> = BinaryOperands<T, &'a ConstValue>;

    impl<'a, T> WithConstOperand<'a, T> {
        #[inline]
        fn konst(&self) -> &ConstValue {
            self.as_flat().1
        }

        #[inline]
        fn is_first_zero(&self) -> bool {
            match self {
                Self::Rev { first, .. } => first.is_zero(),
                _ => false,
            }
        }

        #[inline]
        fn is_second_zero(&self) -> bool {
            match self {
                Self::Orig { second, .. } => second.is_zero(),
                _ => false,
            }
        }

        #[inline]
        fn is_second_one(&self) -> bool {
            match self {
                Self::Orig { second, .. } => second.is_one(),
                _ => false,
            }
        }

        #[inline]
        fn is_first_unsigned_zero(&self) -> bool {
            matches!(
                self,
                BinaryOperands::Rev {
                    first: ConstValue::Int {
                        bit_rep: 0,
                        is_signed: false,
                        ..
                    },
                    ..
                }
            )
        }

        #[inline]
        fn is_second_unsigned_zero(&self) -> bool {
            matches!(
                self,
                BinaryOperands::Orig {
                    second: ConstValue::Int {
                        bit_rep: 0,
                        is_signed: false,
                        ..
                    },
                    ..
                }
            )
        }

        #[inline]
        fn other_into<U>(self) -> U
        where
            T: Into<U>,
        {
            self.flatten().0.into()
        }

        #[inline]
        fn konst_into<U>(self) -> U
        where
            &'a ConstValue: Into<U>,
        {
            self.flatten().1.into()
        }
    }

    impl<Other, Expr> BinaryExprBuilder for ConstSimplifier<Other, Expr>
    where
        for<'a> Expr: From<&'a ConstValue> + From<Other>,
    {
        type ExprRefPair<'a> = WithConstOperand<'a, Other>;
        type Expr<'a> = Result<Expr, Self::ExprRefPair<'a>>;

        impl_general_binary_op_through_singulars!();

        /* NOTE: Most of the implementations cause the match expression to be
         * evaluated multiple times (unless the compiler performs a great job!).
         * Optimize if it becomes a bottleneck.
         */

        fn add<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.konst().is_zero() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn sub<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.is_second_zero() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn mul<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.konst().is_zero() {
                Ok(operands.konst().into())
            } else if operands.konst().is_one() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn div<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.is_first_zero() {
                Ok(operands.konst_into())
            } else if operands.is_second_one() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn rem<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.is_first_zero() {
                Ok(operands.konst_into())
            } else if operands.is_second_one() {
                Ok((&match operands.konst() {
                    ConstValue::Int {
                        size, is_signed, ..
                    } => ConstValue::Int {
                        bit_rep: 0,
                        size: *size,
                        is_signed: *is_signed,
                    },
                    ConstValue::Float { .. } => todo!(),
                    _ => unreachable!("The second operand should be numeric."),
                })
                    .into())
            } else {
                Err(operands)
            }
        }

        fn xor<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.konst().is_zero() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn and<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.konst().is_zero() {
                Ok(operands.konst_into())
            // TODO: All ones case
            } else {
                Err(operands)
            }
        }

        fn or<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.konst().is_zero() {
                Ok(operands.other_into())
            // TODO: All ones case
            } else {
                Err(operands)
            }
        }

        fn shl<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            match operands {
                BinaryOperands::Orig { first, second } if second.is_zero() => Ok(first.into()),
                BinaryOperands::Rev { first, second } if first.is_zero() => Ok(second.into()),
                _ => Err(operands),
            }
        }

        fn shr<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            match operands {
                BinaryOperands::Orig { first, second } if second.is_zero() => Ok(first.into()),
                BinaryOperands::Rev { first, second } if first.is_zero() => Ok(second.into()),
                _ => Err(operands),
            }
        }

        fn eq<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn lt<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.is_second_unsigned_zero() {
                Ok((&ConstValue::Bool(false)).into())
            } else {
                Err(operands)
            }
        }

        fn le<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.is_first_unsigned_zero() {
                Ok((&ConstValue::Bool(true)).into())
            } else {
                Err(operands)
            }
        }

        fn ne<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn ge<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.is_second_unsigned_zero() {
                Ok((&ConstValue::Bool(true)).into())
            } else {
                Err(operands)
            }
        }

        fn gt<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            if operands.is_first_unsigned_zero() {
                Ok((&ConstValue::Bool(false)).into())
            } else {
                Err(operands)
            }
        }

        fn offset<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }
    }

    impl From<&ConstValue> for ValueRef {
        fn from(value: &ConstValue) -> Self {
            value.clone().to_value_ref()
        }
    }
}
