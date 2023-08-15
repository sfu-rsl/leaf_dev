use super::super::alias::ValueRefBinaryExprBuilder;
use super::*;
use crate::abs::{
    expr::{
        macros::*, BinaryExprBuilder, ChainedExprBuilder, CompositeExprBuilder, ExprBuilder,
        UnaryExprBuilder,
    },
    BinaryOp, CastKind, UnaryOp,
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
            checked: bool,
        ) -> Self::Expr<'a> {
            if first.is_symbolic() {
                self.sym_builder.binary_op(
                    BinaryOperands::Orig {
                        first: SymValueRef::new(first),
                        second,
                    },
                    op,
                    checked,
                )
            } else if second.is_symbolic() {
                self.sym_builder.binary_op(
                    BinaryOperands::Rev {
                        first,
                        second: SymValueRef::new(second),
                    },
                    op,
                    checked,
                )
            } else {
                self.conc_builder.binary_op(
                    (ConcreteValueRef::new(first), ConcreteValueRef::new(second)),
                    op,
                    checked,
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

        fn cast<'a>(&mut self, operand: Self::ExprRef<'a>, target: CastKind) -> Self::Expr<'a> {
            call_unary_method!(self, cast, operand, target)
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
            Into::<Value>::into(expr).into()
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

    /// This expression builder is an optimization step that skips generating expressions
    /// when the answer is deterministic based on arithmetics and logics
    ///
    /// For example, when generating an expression for `x * 1`, `mul(x, 1)` will be called,
    /// and ConstSimplifier will return only `x`, not `Expr::Binary { ... }`.
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

    /// This is the base expression builder. It implements the lowest level for
    /// all the binary and unary functions. At this point all optimizations are
    /// considered to be done, so now actual symbolic expressions can be built
    #[derive(Default, Clone)]
    pub(crate) struct CoreBuilder;

    impl BinaryExprBuilder for CoreBuilder {
        type ExprRefPair<'a> = SymBinaryOperands;
        type Expr<'a> = Expr;

        fn binary_op<'a>(
            &mut self,
            operands: Self::ExprRefPair<'a>,
            op: BinaryOp,
            checked: bool,
        ) -> Self::Expr<'a> {
            Expr::Binary {
                operator: op,
                operands,
                checked,
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
            Expr::Len(ProjExprRef::new(operand.into()))
        }

        fn cast<'a>(&mut self, operand: Self::ExprRef<'a>, target: CastKind) -> Self::Expr<'a> {
            match ValueType::try_from(target) {
                Ok(value_type) => Expr::Cast {
                    from: operand,
                    to: value_type,
                },
                Err(target) => {
                    use CastKind::*;
                    match target {
                        PointerUnsize => {
                            match operand.as_ref() {
                                SymValue::Expression(expr) => {
                                    // Nothing special to do currently. Refer to the comment for concrete values.
                                    expr.clone()
                                }
                                SymValue::Variable(_) => unreachable!(
                                    "Symbolic variables are not currently supposed to appear at a pointer/reference position."
                                ),
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
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
            checked: bool,
        ) -> Self::Expr<'a> {
            match (first, second) {
                (ConcreteValue::Const(first_c), ConcreteValue::Const(second_c)) => {
                    ConstValue::binary_op(first_c, second_c, op, checked)
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
                    ConstValue::new_int(
                        len as u128,
                        IntType {
                            bit_size: std::mem::size_of_val(&len) as u64 * 8,
                            is_signed: false,
                        },
                    )
                    .into()
                }
                _ => unreachable!(
                    "Length operation for concrete values is supposed to be called on arrays."
                ),
            }
        }

        fn cast<'a>(
            &mut self,
            operand: Self::ExprRef<'a>,
            target: crate::abs::CastKind,
        ) -> Self::Expr<'a> {
            use CastKind::*;
            match target {
                ToChar | ToInt(_) | ToFloat(_) => match operand {
                    ConcreteValue::Const(c) => match target {
                        ToChar => match c {
                            ConstValue::Int { bit_rep, .. } => {
                                ConstValue::Char(bit_rep.0 as u8 as char).into()
                            }
                            _ => unreachable!(
                                "Casting to char is supposed to happen only on an unsigned byte."
                            ),
                        },
                        ToInt(to) => ConstValue::integer_cast(c, to).into(),
                        ToFloat(to) => todo!("Support casting to float {to:?}"),
                        _ => unreachable!(),
                    },
                    _ => unreachable!("Numeric casts are supposed to happen on a constant."),
                },
                CastKind::PointerUnsize => match operand {
                    ConcreteValue::Ref(_) => {
                        /* Currently, the effect of this operation is at a lower level than our
                         * symbolic state. So we don't need to do anything special.
                         */
                        operand.clone()
                    }
                    _ => unreachable!("Unsize cast is supposed to happen on a reference."),
                },
            }
        }
    }
}

mod simp {
    use super::*;
    use std::marker::PhantomData;

    #[derive(Clone)]
    pub(crate) struct ConstSimplifier<Other, Expr> {
        _phantom: PhantomData<(Other, Expr)>,
    }

    impl<Other, Expr> Default for ConstSimplifier<Other, Expr> {
        fn default() -> Self {
            Self {
                _phantom: PhantomData,
            }
        }
    }

    type WithConstOperand<'a, T> = BinaryOperands<T, &'a ConstValue>;

    impl<'a, T> WithConstOperand<'a, T> {
        /// `konst` is used in place of the word const, since it's a keyword :)
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
                        bit_rep: Wrapping(0),
                        ty: IntType {
                            is_signed: false,
                            ..
                        }
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
                        bit_rep: Wrapping(0),
                        ty: IntType {
                            is_signed: false,
                            ..
                        }
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

        fn add<'a>(&mut self, operands: Self::ExprRefPair<'a>, _checked: bool) -> Self::Expr<'a> {
            // x + 0 = 0 + x = x
            if operands.konst().is_zero() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn sub<'a>(&mut self, operands: Self::ExprRefPair<'a>, _checked: bool) -> Self::Expr<'a> {
            // x - 0 = x
            if operands.is_second_zero() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn mul<'a>(&mut self, operands: Self::ExprRefPair<'a>, _checked: bool) -> Self::Expr<'a> {
            // x * 0 = 0 * x = 0
            if operands.konst().is_zero() {
                Ok(operands.konst().into())
            }
            // x * 1 = x
            else if operands.konst().is_one() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn div<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            // 0 / x = 0
            if operands.is_first_zero() {
                Ok(operands.konst_into())
            }
            // x / 1 = x
            else if operands.is_second_one() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn rem<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            // 0 % x = 0
            if operands.is_first_zero() {
                Ok(operands.konst_into())
            }
            // x % 1 = 0
            else if operands.is_second_one() {
                Ok((&match operands.konst() {
                    ConstValue::Int { ty, .. } => ConstValue::new_int(0_u128, *ty),
                    ConstValue::Float { .. } => todo!(),
                    _ => unreachable!("The second operand should be numeric."),
                })
                    .into())
            } else {
                Err(operands)
            }
        }

        fn xor<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            // x ^ 0 = 0 ^ x = x
            if operands.konst().is_zero() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn and<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            // x & 0 = 0 & x = 0
            if operands.konst().is_zero() {
                Ok(operands.konst_into())
            // TODO: All ones case
            } else {
                Err(operands)
            }
        }

        fn or<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            // x | 0 = 0 | x = x
            if operands.konst().is_zero() {
                Ok(operands.other_into())
            // TODO: All ones case
            } else {
                Err(operands)
            }
        }

        fn shl<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            match operands {
                // x << 0 = x
                BinaryOperands::Orig { first, second } if second.is_zero() => Ok(first.into()),
                // 0 << x = 0
                BinaryOperands::Rev { first, .. } if first.is_zero() => Ok(first.into()),
                _ => Err(operands),
            }
        }

        fn shr<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            match operands {
                // x >> 0 = x
                BinaryOperands::Orig { first, second } if second.is_zero() => Ok(first.into()),
                // 0 >> x = 0
                BinaryOperands::Rev { first, .. } if first.is_zero() => Ok(first.into()),
                _ => Err(operands),
            }
        }

        fn eq<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn lt<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            // For unsigned integers, nothing is less than zero.
            if operands.is_second_unsigned_zero() {
                Ok((&ConstValue::Bool(false)).into())
            } else {
                Err(operands)
            }
        }

        fn le<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            // For unsigned integers, zero is less than or equal to everything.
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
            // For unsigned integers, everything is greater than or equal to zero.
            if operands.is_second_unsigned_zero() {
                Ok((&ConstValue::Bool(true)).into())
            } else {
                Err(operands)
            }
        }

        fn gt<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            // For unsigned integers, zero is not greater than anything.
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
