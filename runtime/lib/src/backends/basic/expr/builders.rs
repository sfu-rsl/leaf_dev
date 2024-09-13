use super::super::alias::ValueRefBinaryExprBuilder;
use super::{UnaryOp as BasicUnaryOp, *};
use crate::abs::{
    expr::{
        macros::*, BinaryExprBuilder, ChainedExprBuilder, CompositeExprBuilder, ExprBuilder,
        UnaryExprBuilder,
    },
    BinaryOp, CastKind, UnaryOp,
};
use common::log_debug;

type Composite<Binary, Unary> = CompositeExprBuilder<Binary, Unary>;
type Chained<Current, Next, Expr = ValueRef, CurrentExpr = Expr> =
    ChainedExprBuilder<Current, Next, Expr, CurrentExpr>;

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
    use super::{concrete::ConcreteAbstractorBuilder, symbolic::SymbolicBuilder, *};

    /// An expression builder that separates the path for expressions that involve symbolic values,
    /// or the ones that are fully based on concrete values.
    /// NOTE: In an ideal case, fully concrete expressions should not be asked to created. So in
    /// the future, this top-level builder will be reduced to the symbolic builder.
    #[derive(Default)]
    pub(crate) struct TopLevelBuilder {
        sym_builder: SymbolicBuilder,
        conc_builder: ConcreteAbstractorBuilder,
    }

    impl BinaryExprBuilder for TopLevelBuilder {
        type ExprRefPair<'a> = (ValueRef, ValueRef);
        type Expr<'a> = ValueRef;

        fn binary_op<'a>(
            &mut self,
            (first, second): Self::ExprRefPair<'a>,
            op: BinaryOp,
        ) -> Self::Expr<'a> {
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
                        first,
                        second: SymValueRef::new(second),
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

        fn ptr_metadata<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            call_unary_method!(self, ptr_metadata, operand)
        }

        fn address_of<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            call_unary_method!(self, address_of, operand)
        }

        fn len<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            call_unary_method!(self, len, operand)
        }

        fn discriminant<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            call_unary_method!(self, discriminant, operand)
        }

        fn cast<'a>(&mut self, operand: Self::ExprRef<'a>, target: CastKind) -> Self::Expr<'a> {
            call_unary_method!(self, cast, operand, target)
        }
    }
}

mod symbolic {
    use super::{adapters::ConstFolder, adapters::ConstSimplifier, core::CoreBuilder, *};

    pub(crate) type SymbolicBuilder = Composite<
        /*Binary:*/
        Chained<LazyEvaluatorBuilder, Chained<ConstSimplifier, Chained<ConstFolder, CoreBuilder>>>,
        /*Unary:*/ CoreBuilder,
    >;

    #[derive(Default)]
    pub(crate) struct LazyEvaluatorBuilder;

    impl BinaryExprBuilder for LazyEvaluatorBuilder {
        type ExprRefPair<'a> = SymBinaryOperands;
        type Expr<'a> = Result<ValueRef, SymBinaryOperands>;

        fn binary_op<'a>(
            &mut self,
            operands: Self::ExprRefPair<'a>,
            _op: BinaryOp,
        ) -> Self::Expr<'a> {
            match operands.as_flat().1.as_ref() {
                Value::Concrete(ConcreteValue::Unevaluated(UnevalValue::Lazy(lazy))) => {
                    let value = unsafe {
                        lazy.try_retrieve_as_scalar(None)
                            .expect(
                                concat!(
                                    "The value participating in a binary expression is expected to be a scalar. ",
                                    "Maybe the value type is not available?"
                                ),
                            )
                    }.to_value_ref();
                    let mut operands = operands.flatten();
                    operands.1 = value;
                    Err(Self::ExprRefPair::from(operands))
                }
                _ => Err(operands),
            }
        }

        impl_singular_binary_ops_through_general!();
    }
}

mod adapters {
    use derive_more::{Deref, DerefMut};

    use crate::abs::expr::variance::*;

    use super::*;

    use BinaryExprBuilder as BEB;
    use UnaryExprBuilder as UEB;

    impl From<Expr> for ValueRef {
        fn from(expr: Expr) -> Self {
            Into::<Value>::into(expr).to_value_ref()
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

    /// This expression builder is an optimization step that folds constants in
    /// the expressions if possible and avoids creating a new expression.
    ///
    /// For example, when generating an expression for `x * 2 * 3`,
    /// `mul(mul(x, 1), 2)` will be called, and ConstFolder will return the
    /// result equivalent to `x * 6` instead of ((x * 2) * 3).
    #[derive(Default, Clone, Deref, DerefMut)]
    pub(crate) struct ConstFolder(simp::ConstFolder);

    impl BinaryExprBuilderAdapter for ConstFolder {
        type TargetExprRefPair<'a> = SymBinaryOperands;
        type TargetExpr<'a> = Result<ValueRef, SymBinaryOperands>;

        fn adapt<'t, F>(operands: Self::TargetExprRefPair<'t>, build: F) -> Self::TargetExpr<'t>
        where
            F: for<'s> FnH<<Self::Target as BEB>::ExprRefPair<'s>, <Self::Target as BEB>::Expr<'s>>,
        {
            /* If this is a binary operation between a constant and a binary
             * expression with a constant operand.
             */
            let (first, second, is_reversed) = operands.as_flat();
            if let Value::Concrete(ConcreteValue::Const(b)) = second.as_ref() {
                if let SymValue::Expression(Expr::Binary(first)) = first.as_ref() {
                    let (x, a, is_inner_reversed) = first.operands.as_flat();
                    if let Value::Concrete(ConcreteValue::Const(a)) = a.as_ref() {
                        if let Ok(result) = build(
                            (
                                BinaryExpr {
                                    operands: (x, a, is_inner_reversed).into(),
                                    operator: first.operator,
                                },
                                b,
                                is_reversed,
                            )
                                .into(),
                        ) {
                            return Ok(result.to_value_ref().0);
                        }
                    }
                }
            }

            Err(operands)
        }
    }
}

mod core {
    use super::*;
    use std::mem::size_of;

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
        ) -> Self::Expr<'a> {
            Expr::Binary(BinaryExpr {
                operator: op,
                operands,
            })
        }

        impl_singular_binary_ops_through_general!();
    }

    impl UnaryExprBuilder for CoreBuilder {
        type ExprRef<'a> = SymValueRef;
        type Expr<'a> = Expr;

        impl_general_unary_op_through_singulars!();

        fn neg<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            Expr::Unary {
                operator: BasicUnaryOp::Neg,
                operand,
            }
        }

        fn not<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            Expr::Unary {
                operator: BasicUnaryOp::Not,
                operand,
            }
        }

        fn ptr_metadata<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            Expr::Projection(
                ProjExpr::SymHost(SymHostProj {
                    host: operand.into(),
                    kind: ProjKind::Field(FieldAccessKind::PtrMetadata),
                    metadata: todo!("#443"),
                })
                .into(),
            )
        }

        fn address_of<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            Expr::Ref(ProjExprRef::new(operand.into()))
        }

        fn len<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            Expr::Len(ProjExprRef::new(operand.into()))
        }

        fn discriminant<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            todo!("#233: Add support for discriminant operator {:?}", operand)
        }

        fn cast<'a>(&mut self, operand: Self::ExprRef<'a>, target: CastKind) -> Self::Expr<'a> {
            log_debug!("Casting {:?} to {:?}", operand, target);
            let expr = match ValueType::try_from(target) {
                Ok(value_type) => to_cast_expr(operand, value_type),
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
                        ExposeProvenance | ToPointer(_) => {
                            todo!("#331: Add support for casting symbolic pointers")
                        }
                        SizedDynamize => {
                            todo!("#317: Add support for dyn* cast of symbolic values")
                        }
                        Transmute(dst_ty_id) => {
                            /* NOTE: Should we check for primitive types?
                             * Probably not. Numeric types are converted to their actual cast type
                             * by the compiler. */
                            ProjExpr::SymHost(SymHostProj {
                                host: operand.into(),
                                kind: ProjKind::Downcast(DowncastKind::Transmutation(dst_ty_id)),
                                // NOTE: We should see if we are going to use host's metadata at all.
                                metadata: ProjMetadata::unknown(),
                            })
                            .into()
                        }
                        _ => unreachable!(),
                    }
                }
            };
            log_debug!("Cast expression for {:?}: {}", target, expr);

            expr
        }
    }

    const CHAR_BIT_SIZE: u32 = size_of::<char>() as u32 * 8;
    const TO_CHAR_BIT_SIZE: u32 = size_of::<u8>() as u32 * 8; // Can only cast to a char from a u8

    fn to_cast_expr(from: SymValueRef, to: ValueType) -> Expr {
        let from_type = match ValueType::try_from(from.as_ref()) {
            Ok(value_type) => value_type,
            Err(value) => unimplemented!("Casting from {} to {} is not supported.", value, to),
        };

        match to {
            ValueType::Char => {
                debug_assert_eq!(
                    from_type,
                    ValueType::Int(IntType {
                        bit_size: 8,
                        is_signed: false
                    }),
                    "Casting from {from_type} to char is not supported."
                );
                Expr::Extension {
                    source: from,
                    is_zero_ext: true,
                    bits_to_add: CHAR_BIT_SIZE - TO_CHAR_BIT_SIZE,
                    ty: to,
                }
            }
            ValueType::Int(IntType {
                bit_size,
                is_signed,
            }) => match from_type {
                ValueType::Bool => Expr::Ite {
                    condition: from,
                    if_target: ConstValue::new_int(
                        1 as u128,
                        IntType {
                            bit_size,
                            is_signed,
                        },
                    )
                    .to_value_ref(),
                    else_target: ConstValue::new_int(
                        0 as u128,
                        IntType {
                            bit_size,
                            is_signed,
                        },
                    )
                    .to_value_ref(),
                },
                ValueType::Char => {
                    if bit_size as u32 > CHAR_BIT_SIZE {
                        return Expr::Extension {
                            source: from,
                            is_zero_ext: true,
                            bits_to_add: bit_size as u32 - CHAR_BIT_SIZE,
                            ty: to,
                        };
                    } else {
                        return Expr::Extraction {
                            source: from,
                            high: bit_size as u32 - 1,
                            low: 0,
                            ty: to,
                        };
                    }
                }
                ValueType::Int(IntType {
                    bit_size: from_bit_size,
                    is_signed: is_from_signed,
                }) => {
                    if bit_size > from_bit_size {
                        let bits_to_add = bit_size - from_bit_size;
                        return Expr::Extension {
                            source: from,
                            is_zero_ext: !is_from_signed,
                            bits_to_add: bits_to_add as u32,
                            ty: to,
                        };
                    } else {
                        return Expr::Extraction {
                            source: from,
                            high: bit_size as u32 - 1,
                            low: 0,
                            ty: to,
                        };
                    }
                }
                ValueType::Float { .. } => todo!(),
            },
            ValueType::Float { .. } => todo!(),
            _ => unreachable!("Casting from {from_type} to {to} is not supported."),
        }
    }
}

mod concrete {
    use super::*;

    #[derive(Default)]
    pub(crate) struct ConcreteAbstractorBuilder;

    impl BinaryExprBuilder for ConcreteAbstractorBuilder {
        type ExprRefPair<'a> = (ConcreteValueRef, ConcreteValueRef);
        type Expr<'a> = ValueRef;

        fn binary_op<'a>(
            &mut self,
            _operands: Self::ExprRefPair<'a>,
            _op: BinaryOp,
        ) -> Self::Expr<'a> {
            UnevalValue::Some.to_value_ref()
        }

        impl_singular_binary_ops_through_general!();
    }

    impl UnaryExprBuilder for ConcreteAbstractorBuilder {
        type ExprRef<'a> = ConcreteValueRef;
        type Expr<'a> = ValueRef;

        fn unary_op<'a>(&mut self, _operand: Self::ExprRef<'a>, _op: UnaryOp) -> Self::Expr<'a> {
            UnevalValue::Some.to_value_ref()
        }

        impl_singular_unary_ops_through_general!();

        fn address_of<'a>(&mut self, _operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            UnevalValue::Some.to_value_ref()
        }

        fn len<'a>(&mut self, _operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            UnevalValue::Some.to_value_ref()
        }

        fn discriminant<'a>(&mut self, _operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            UnevalValue::Some.to_value_ref()
        }

        fn cast<'a>(&mut self, _operand: Self::ExprRef<'a>, ـtarget: CastKind) -> Self::Expr<'a> {
            UnevalValue::Some.to_value_ref()
        }
    }
}

mod simp {
    use super::*;
    use std::marker::PhantomData;

    /// # Generic Parameters
    /// * `Other`: The type of the other operand in an operation with a constant.
    /// * `ResultExpr`: The type of the result expression which effectively will
    ///   be either a constant or the other expression itself.
    #[derive(Clone)]
    pub(crate) struct ConstSimplifier<Other = SymValueRef, ResultExpr = ValueRef> {
        _phantom: PhantomData<(Other, ResultExpr)>,
    }

    impl<Other, ResultExpr> Default for ConstSimplifier<Other, ResultExpr> {
        fn default() -> Self {
            Self {
                _phantom: PhantomData,
            }
        }
    }

    /// A pair of operands where one of them is a constant.
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

    impl<Other, ResultExpr> BinaryExprBuilder for ConstSimplifier<Other, ResultExpr>
    where
        for<'a> ResultExpr: From<&'a ConstValue> + From<Other>,
    {
        type ExprRefPair<'a> = WithConstOperand<'a, Other>;
        type Expr<'a> = Result<ResultExpr, Self::ExprRefPair<'a>>;

        impl_general_binary_op_through_singulars!();

        /* NOTE: Most of the implementations cause the match expression to be
         * evaluated multiple times (unless the compiler performs a great job!).
         * Optimize if it becomes a bottleneck.
         */

        fn add<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            // x + 0 = 0 + x = x
            if operands.konst().is_zero() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn add_unchecked<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.add(operands)
        }

        fn add_with_overflow<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.add(operands)
        }

        fn sub<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            // x - 0 = x
            if operands.is_second_zero() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn sub_unchecked<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.sub(operands)
        }

        fn sub_with_overflow<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.sub(operands)
        }

        fn mul<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            // x * 0 = 0 * x = 0
            if operands.konst().is_zero() {
                Ok(operands.konst().into())
            }
            // x * 1 = 1 * x = x
            else if operands.konst().is_one() {
                Ok(operands.other_into())
            } else {
                Err(operands)
            }
        }

        fn mul_unchecked<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.mul(operands)
        }

        fn mul_with_overflow<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.mul(operands)
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

        fn shl_unchecked<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.shl(operands)
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

        fn shr_unchecked<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.shr(operands)
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

        fn cmp<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
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

    #[derive(Clone, Default)]
    pub(crate) struct ConstFolder;

    type FoldableOperands<'a> =
        WithConstOperand<'a, BinaryExpr<WithConstOperand<'a, &'a SymValueRef>>>;

    impl<'a> FoldableOperands<'a> {
        #[inline]
        /// Returns the constant of the inner expression
        fn a(&self) -> &ConstValue {
            self.as_flat().0.operands.as_flat().1
        }

        #[inline]
        /// Returns the constant of the outer expression
        fn b(&self) -> &ConstValue {
            self.as_flat().1
        }

        #[inline]
        fn expr(&self) -> &BinaryExpr<WithConstOperand<'a, &'a SymValueRef>> {
            self.as_flat().0
        }

        /// Creates a new BinaryExpr from the inner expression and the folded value.
        /// Uses the original operator of the inner expression and uses the folded value
        /// for the constant.
        fn fold_expr(self, folded_value: ConstValue) -> BinaryExpr {
            let expr = self.flatten().0;
            let (x, _, is_reversed) = expr.operands.flatten();
            BinaryExpr {
                operands: SymBinaryOperands::from((
                    x.clone(),
                    folded_value.to_value_ref(),
                    is_reversed,
                )),
                operator: expr.operator,
            }
        }

        /// Creates a new BinaryExpr from the existing symbolic value and the newly folded
        /// constant. Accepts a new operator and the `is_reversed` flag (true means x on the
        /// right).
        fn new_expr(self, folded_value: ConstValue, op: BinaryOp, is_reversed: bool) -> BinaryExpr {
            let expr = self.flatten().0;
            let x = expr.operands.flatten().0;
            BinaryExpr {
                operands: SymBinaryOperands::from((
                    x.clone(),
                    folded_value.to_value_ref(),
                    is_reversed,
                )),
                operator: op,
            }
        }
    }

    impl BinaryExprBuilder for ConstFolder {
        // In general, we don't need to worry about if the constants are signed. Since the value
        // is stored in a u128, the result will be correct once the value is converted back to
        // the original type. The same goes for overflow and underflow since this code is only
        // reached when the source is compiled with optimizations in which case overflow and
        // underflow are performed anyway.

        type ExprRefPair<'a> = FoldableOperands<'a>;
        type Expr<'a> = Result<BinaryExpr, Self::ExprRefPair<'a>>;

        impl_general_binary_op_through_singulars!();

        fn add<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            let (a, b) = (operands.a(), operands.b());

            match operands.expr().operator {
                // (x + a) + b = x + (a + b)
                BinaryOp::Add => {
                    let folded_value = ConstValue::binary_op_arithmetic(a, b, BinaryOp::Add);
                    Ok(operands.fold_expr(folded_value))
                }
                BinaryOp::Sub => {
                    match &operands.expr().operands {
                        // (x - a) + b = x - (a - b)
                        BinaryOperands::Orig { .. } => {
                            let folded_value =
                                ConstValue::binary_op_arithmetic(a, b, BinaryOp::Sub);
                            Ok(operands.fold_expr(folded_value))
                        }
                        // (a - x) + b = (a + b) - x
                        BinaryOperands::Rev { .. } => {
                            let folded_value =
                                ConstValue::binary_op_arithmetic(a, b, BinaryOp::Add);
                            Ok(operands.fold_expr(folded_value))
                        }
                    }
                }
                _ => Err(operands),
            }
        }

        fn add_unchecked<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.add(operands)
        }

        fn sub<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            let (a, b) = (operands.a(), operands.b());

            match operands {
                BinaryOperands::Orig { .. } => {
                    match operands.expr().operator {
                        // (x + a) - b = x + (a - b)
                        BinaryOp::Add => {
                            let folded_value =
                                ConstValue::binary_op_arithmetic(a, b, BinaryOp::Sub);
                            Ok(operands.fold_expr(folded_value))
                        }
                        BinaryOp::Sub => {
                            match operands.expr().operands {
                                // (x - a) - b = x - (a + b)
                                BinaryOperands::Orig { .. } => {
                                    let folded_value =
                                        ConstValue::binary_op_arithmetic(a, b, BinaryOp::Add);
                                    Ok(operands.fold_expr(folded_value))
                                }
                                // (a - x) - b = (a - b) - x
                                BinaryOperands::Rev { .. } => {
                                    let folded_value =
                                        ConstValue::binary_op_arithmetic(a, b, BinaryOp::Sub);
                                    Ok(operands.fold_expr(folded_value))
                                }
                            }
                        }
                        _ => Err(operands),
                    }
                }
                BinaryOperands::Rev { .. } => match operands.expr().operator {
                    // b - (x + a) = (b - a) - x
                    BinaryOp::Add => {
                        let folded_value = ConstValue::binary_op_arithmetic(b, a, BinaryOp::Sub);
                        Ok(operands.new_expr(folded_value, BinaryOp::Sub, true))
                    }
                    BinaryOp::Sub => {
                        match operands.expr().operands {
                            // b - (x - a) = (b + a) - x
                            BinaryOperands::Orig { .. } => {
                                let folded_value =
                                    ConstValue::binary_op_arithmetic(b, a, BinaryOp::Add);
                                Ok(operands.new_expr(folded_value, BinaryOp::Sub, true))
                            }
                            // b - (a - x) = x + (b - a)
                            BinaryOperands::Rev { .. } => {
                                let folded_value =
                                    ConstValue::binary_op_arithmetic(b, a, BinaryOp::Sub);
                                Ok(operands.new_expr(folded_value, BinaryOp::Add, false))
                            }
                        }
                    }
                    _ => Err(operands),
                },
            }
        }

        fn sub_unchecked<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.sub(operands)
        }

        fn mul<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            let (a, b) = (operands.a(), operands.b());

            match operands.expr().operator {
                // (x * a) * b = x * (a * b)
                BinaryOp::Mul => {
                    let folded_value = ConstValue::binary_op_arithmetic(a, b, BinaryOp::Mul);
                    Ok(operands.fold_expr(folded_value))
                }
                _ => Err(operands),
            }
        }

        fn mul_unchecked<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.mul(operands)
        }

        fn div<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn rem<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn and<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            let (a, b) = (operands.a(), operands.b());

            match operands.expr().operator {
                // (x & a) & b = x & (a & b)
                BinaryOp::BitAnd => {
                    let folded_value = ConstValue::binary_op_arithmetic(a, b, BinaryOp::BitAnd);
                    Ok(operands.fold_expr(folded_value))
                }
                _ => Err(operands),
            }
        }

        fn or<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            let (a, b) = (operands.a(), operands.b());

            match operands.expr().operator {
                // (x | a) | b = x | (a | b)
                BinaryOp::BitOr => {
                    let folded_value = ConstValue::binary_op_arithmetic(a, b, BinaryOp::BitOr);
                    Ok(operands.fold_expr(folded_value))
                }
                _ => Err(operands),
            }
        }

        fn xor<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            let (a, b) = (operands.a(), operands.b());

            match operands.expr().operator {
                // (x ^ a) ^ b = x ^ (a ^ b)
                BinaryOp::BitXor => {
                    let folded_value = ConstValue::binary_op_arithmetic(a, b, BinaryOp::BitXor);
                    Ok(operands.fold_expr(folded_value))
                }
                _ => Err(operands),
            }
        }

        fn shl<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            let (a, b) = (operands.a(), operands.b());

            match operands.expr().operator {
                // (x << a) << b = x << (a + b)
                BinaryOp::Shl => {
                    let folded_value = ConstValue::binary_op_arithmetic(a, b, BinaryOp::Add);
                    Ok(operands.fold_expr(folded_value))
                }
                _ => Err(operands),
            }
        }

        fn shl_unchecked<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.shl(operands)
        }

        fn shr<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            let (a, b) = (operands.a(), operands.b());

            match operands.expr().operator {
                // (x >> a) >> b = x >> (a + b)
                BinaryOp::Shr => {
                    let folded_value = ConstValue::binary_op_arithmetic(a, b, BinaryOp::Add);
                    Ok(operands.fold_expr(folded_value))
                }
                _ => Err(operands),
            }
        }

        fn shr_unchecked<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            self.shr(operands)
        }

        fn add_with_overflow<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn sub_with_overflow<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn mul_with_overflow<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn eq<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn ne<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn lt<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn le<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn gt<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn ge<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn cmp<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }

        fn offset<'a>(&mut self, operands: Self::ExprRefPair<'a>) -> Self::Expr<'a> {
            Err(operands)
        }
    }
}
