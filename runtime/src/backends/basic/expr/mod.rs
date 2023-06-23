pub(super) mod builders;
pub(super) mod proj;
pub(super) mod translators;
pub(crate) mod utils;

use std::{assert_matches::assert_matches, num::Wrapping, ops::Deref, rc::Rc};

use crate::abs::{BinaryOp, FieldIndex, FloatType, IntType, UnaryOp, ValueType, VariantIndex};

use self::utils::define_reversible_pair;

use super::place::Place;

pub(crate) type ValueRef = Rc<Value>;
pub(crate) type ConcreteValueRef = ConcreteValueGuard<ValueRef>;
pub(crate) type ConcreteValueMutRef<'a> = ConcreteValueGuard<&'a mut ValueRef>;
pub(crate) type SymValueRef = SymValueGuard<ValueRef>;
pub(crate) type SymVarId = u32;

#[derive(Clone, Debug)]
pub(crate) enum Value {
    Concrete(ConcreteValue),
    Symbolic(SymValue),
}

impl Value {
    pub(crate) fn is_symbolic(&self) -> bool {
        matches!(self, Value::Symbolic(_))
    }
}

#[derive(Clone, Debug)]
pub(crate) enum ConcreteValue {
    Const(ConstValue),
    Adt(AdtValue),
    Array(ArrayValue),
    Ref(RefValue),
}

impl ConcreteValue {
    pub fn from_const(value: ConstValue) -> Self {
        Self::Const(value)
    }
}

// FIXME: Remove this error suppression after adding support for floats.
#[allow(unused)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum ConstValue {
    Bool(bool),
    Char(char),
    Int {
        bit_rep: Wrapping<u128>,
        ty: IntType,
    },
    Float {
        bit_rep: u128,
        ty: FloatType,
    },
    Str(&'static str),
    Func(u64),
}

impl ConstValue {
    pub fn new_int<T: Into<u128>>(value: T, ty: IntType) -> Self {
        Self::Int {
            bit_rep: Wrapping(value.into()),
            ty,
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Self::Bool(value) => !value,
            Self::Int { bit_rep, .. } => *bit_rep == Wrapping(0),
            Self::Float { .. } => todo!(),
            _ => unreachable!("Only numerical values can be checked for zero."),
        }
    }

    pub fn is_one(&self) -> bool {
        match self {
            Self::Bool(value) => *value,
            Self::Int { bit_rep, .. } => *bit_rep == Wrapping(1),
            Self::Float { .. } => todo!(),
            _ => unreachable!("Only numerical values can be checked for one."),
        }
    }

    pub fn unary_op(this: &Self, operator: UnaryOp) -> ConstValue {
        match operator {
            UnaryOp::Neg => match this {
                Self::Int {
                    bit_rep,
                    ty:
                        ty @ IntType {
                            is_signed: true, ..
                        },
                } => Self::Int {
                    bit_rep: todo!("Proposed value: {}", !bit_rep + Wrapping(1)),
                    ty: *ty,
                },
                Self::Float { .. } => unimplemented!(),
                _ => unreachable!("Negation is meant only for signed integers and floats."),
            },
            UnaryOp::Not => match this {
                Self::Bool(value) => Self::Bool(!value),
                Self::Int { bit_rep, ty } => Self::Int {
                    bit_rep: !bit_rep,
                    ty: *ty,
                },
                _ => unreachable!("Not operand only works on boolean and integers."),
            },
        }
    }

    pub fn binary_op(
        first: &Self,
        second: &Self,
        operator: BinaryOp,
        checked: bool,
    ) -> ConcreteValue {
        match operator {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::BitXor
            | BinaryOp::BitAnd
            | BinaryOp::BitOr => {
                if checked {
                    let res = ConcreteValue::Adt(Self::binary_op_checked_arithmetic(
                        first, second, operator,
                    ));
                    println!("checked_arithmetic result: {:?}", res);
                    res
                } else {
                    ConcreteValue::Const(Self::binary_op_arithmetic(first, second, operator))
                }
            }
            BinaryOp::Shl | BinaryOp::Shr => {
                ConcreteValue::Const(Self::binary_op_shift(first, second, operator))
            }
            BinaryOp::Eq
            | BinaryOp::Lt
            | BinaryOp::Le
            | BinaryOp::Ne
            | BinaryOp::Ge
            | BinaryOp::Gt => ConcreteValue::Const(ConstValue::Bool(Self::binary_op_cmp(
                first, second, operator,
            ))),
            _ => unimplemented!("{:?} {:?} {:?}", first, second, operator),
        }
    }

    pub fn integer_cast(this: &Self, to: IntType) -> Self {
        match this {
            /* This seems overly simple but when the number is originally cast to the u128 to get its bit representation,
             * this covers any of the casting that would need to be done here. If the original number was unsigned then
             * the leading bits of the u128 will be 0s and if it was signed then the leading bits will be 1s to handle
             * the sign extension. Now here when we track the actual cast that needs to be done, the target type has at
             * most 128 bits so we can just truncate the leading bits to get the correct bit representation. The
             * truncation is handled by just recording the size of the target type.
             */
            Self::Int { bit_rep, .. } => Self::Int {
                bit_rep: *bit_rep,
                ty: to,
            },
            Self::Bool(value) => Self::Int {
                bit_rep: Wrapping(*value as u128),
                ty: to,
            },
            Self::Char(value) => Self::Int {
                bit_rep: Wrapping(*value as u128),
                ty: to,
            },
            Self::Float { .. } => todo!("Casting float to integer is not implemented yet."),
            _ => unreachable!("Casting {this:?} to integer is not possible."),
        }
    }

    fn binary_op_arithmetic(first: &Self, second: &Self, operator: BinaryOp) -> Self {
        match (first, second) {
            (
                Self::Int {
                    bit_rep: first,
                    ty: first_ty,
                },
                Self::Int {
                    bit_rep: second,
                    ty: second_ty,
                },
            ) => {
                assert_eq!(first_ty.bit_size, second_ty.bit_size);

                let result = match operator {
                    BinaryOp::Add => first + second,
                    BinaryOp::Sub => first - second,
                    BinaryOp::Mul => first * second,
                    BinaryOp::Div => first / second,
                    BinaryOp::Rem => first % second,
                    BinaryOp::BitXor => first ^ second,
                    BinaryOp::BitAnd => first & second,
                    BinaryOp::BitOr => first | second,
                    _ => unreachable!(),
                };

                Self::Int {
                    bit_rep: result,
                    ty: *first_ty,
                }
            }

            (Self::Float { .. }, Self::Float { .. }) => unimplemented!(),

            (Self::Bool(first_value), Self::Bool(second_value)) => {
                let result = match operator {
                    BinaryOp::BitXor => first_value ^ second_value,
                    BinaryOp::BitAnd => first_value & second_value,
                    BinaryOp::BitOr => first_value | second_value,
                    _ => unreachable!(),
                };
                Self::Bool(result)
            }

            _ => unreachable!(),
        }
    }

    fn binary_op_checked_arithmetic(first: &Self, second: &Self, operator: BinaryOp) -> AdtValue {
        /// use logic to determine whether the operation will overflow or underflow or be in bounds
        fn checked_op(
            operator: BinaryOp,
            first: u128,
            second: u128,
            IntType {
                bit_size,
                is_signed,
            }: IntType, // pattern matching in function args, cool!
        ) -> Option<u128> {
            // TODO: please double check that my bit twiddling never accidentally overflows
            if is_signed {
                let first = ConstValue::as_signed(first, bit_size);
                let second = ConstValue::as_signed(second, bit_size);

                let result = match operator {
                    BinaryOp::Add => first.checked_add(second),
                    BinaryOp::Sub => first.checked_sub(second),
                    BinaryOp::Mul => first.checked_mul(second),
                    _ => unreachable!("unsupported by rust"),
                };

                if let Some(result) = result {
                    // case: i128 has not overflowed, so result is valid. Check if we're
                    // higher than our type's max value or lower than it's min value.
                    let max = ((1 << (bit_size as u128 - 1)) - 1) as i128;
                    let min = match bit_size {
                        0..=127 => -(1 << (bit_size as i128 - 1)),
                        128 => i128::MIN,
                        129..=u64::MAX => panic!("unsupported integer size; too large"),
                    };
                    if result > max || result < min {
                        None
                    } else {
                        Some(ConstValue::as_unsigned(result, bit_size))
                    }
                } else {
                    // case: i128 overflowed, so any smaller type also overflowed
                    None
                }
            } else {
                let result = match operator {
                    BinaryOp::Add => first.checked_add(second),
                    BinaryOp::Sub => first.checked_sub(second),
                    BinaryOp::Mul => first.checked_mul(second),
                    _ => unreachable!("unsupported by rust"),
                };

                if let Some(result) = result {
                    // case: u128 has not overflowed, check if we're higher than our max
                    let max = match bit_size {
                        0..=127 => (1 << (bit_size as u128)) - 1,
                        128 => u128::MAX,
                        129..=u64::MAX => panic!("unsupported integer size; too large"),
                    };
                    if result > max { None } else { Some(result) }
                } else {
                    // case: u128 overflowed, so any smaller type also overflowed
                    None
                }
            }
        }

        // the rust docs claims that only integers are supported:
        // https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/mir/syntax/enum.Rvalue.html#variant.CheckedBinaryOp
        match (first, second) {
            (
                Self::Int {
                    bit_rep: first,
                    ty:
                        ty @ IntType {
                            bit_size: first_size,
                            is_signed: first_signed,
                        },
                },
                Self::Int {
                    bit_rep: second,
                    ty:
                        IntType {
                            bit_size: second_size,
                            ..
                        },
                },
            ) => {
                // TODO: what if first is signed & second is not?
                assert_eq!(*first_size, *second_size);

                // TODO: integrate wrapped arithmetic
                let result = checked_op(operator, first.0, second.0, *ty);
                match result {
                    Some(result) => {
                        let ty = IntType {
                            bit_size: *first_size,
                            is_signed: *first_signed,
                        };
                        AdtValue::checked_success(result, ty)
                    }
                    None => AdtValue::checked_overflow(),
                }
            }
            _ => unreachable!("only integers are supported by rust"),
        }
    }

    fn binary_op_shift(first: &Self, second: &Self, operator: BinaryOp) -> Self {
        match (first, second) {
            (
                Self::Int {
                    bit_rep: first,
                    ty: first_ty,
                },
                Self::Int {
                    bit_rep: second,
                    ty: second_ty,
                },
            ) => {
                let result = match operator {
                    BinaryOp::Shl => {
                        assert!(
                            !second_ty.is_signed || Self::is_positive(second.0, second_ty.bit_size),
                            "Shifting by a negative value is not expected."
                        ); //TODO we can get rid of this assertion in the future
                        first << second.0 as usize
                    }
                    BinaryOp::Shr => {
                        assert!(
                            !second_ty.is_signed || Self::is_positive(second.0, second_ty.bit_size),
                            "Shifting by a negative value is not expected."
                        ); //TODO we can get rid of this assertion in the future
                        first >> second.0 as usize
                    }

                    _ => unreachable!(),
                };

                Self::Int {
                    bit_rep: result,
                    ty: *first_ty,
                }
            }
            _ => unreachable!("Shifting is only possible on integers."),
        }
    }

    fn binary_op_cmp(first: &Self, second: &Self, operator: BinaryOp) -> bool {
        match operator {
            BinaryOp::Eq => first.eq(second),
            BinaryOp::Ne => first.ne(second),
            _ => match (first, second) {
                (
                    Self::Int {
                        bit_rep: first,
                        ty: first_ty,
                    },
                    Self::Int {
                        bit_rep: second,
                        ty: second_ty,
                    },
                ) => {
                    assert_eq!(*first_ty, *second_ty);

                    if !first_ty.is_signed {
                        return match operator {
                            BinaryOp::Lt => first < second,
                            BinaryOp::Le => first <= second,
                            BinaryOp::Ge => first >= second,
                            BinaryOp::Gt => first > second,
                            _ => unreachable!(),
                        };
                    }

                    let signs = (
                        Self::is_positive(first.0, first_ty.bit_size),
                        Self::is_positive(second.0, second_ty.bit_size),
                    );

                    match signs {
                        (true, true) => match operator {
                            BinaryOp::Lt => first < second,
                            BinaryOp::Le => first <= second,
                            BinaryOp::Ge => first >= second,
                            BinaryOp::Gt => first > second,
                            _ => unreachable!(),
                        },

                        (false, false) => {
                            let first = Self::as_signed(first.0, first_ty.bit_size);
                            let second = Self::as_signed(second.0, second_ty.bit_size);
                            match operator {
                                BinaryOp::Lt => first < second,
                                BinaryOp::Le => first <= second,
                                BinaryOp::Ge => first >= second,
                                BinaryOp::Gt => first > second,
                                _ => unreachable!(),
                            }
                        }

                        (false, true) => match operator {
                            BinaryOp::Lt => true,
                            BinaryOp::Le => true,
                            BinaryOp::Ge => false,
                            BinaryOp::Gt => false,
                            _ => unreachable!(),
                        },
                        (true, false) => match operator {
                            BinaryOp::Lt => false,
                            BinaryOp::Le => false,
                            BinaryOp::Ge => true,
                            BinaryOp::Gt => true,
                            _ => unreachable!(),
                        },
                    }
                }

                (Self::Float { .. }, Self::Float { .. }) => unimplemented!(),

                _ => unimplemented!(),
            },
        }
    }

    fn is_positive(bit_rep: u128, size: u64) -> bool {
        let mask: u128 = 1 << (size - 1);
        bit_rep & mask == 0
    }

    fn as_signed(bit_rep: u128, size: u64) -> i128 {
        let mask: u128 = (1 << (size - 1)) - 1; // Create a mask of 1s with the given size except the sign bit
        let sign_mask: u128 = 1 << (size - 1); // Create a mask for the sign bit, for example 1000...0
        let sign_extend: u128 = (!mask) & sign_mask; // Create a sign extension mask, it would be something like 000...010...000
        // assert!(sign_extended == sign_mask); // i believe this is the case?

        let sign_bit: bool = (bit_rep & sign_mask) != 0;
        let value = bit_rep & mask;

        let signed_value = if sign_bit {
            (value | sign_extend) as i128
        } else {
            value as i128
        };

        signed_value
    }

    /// convert a signed value into its bit representation
    fn as_unsigned(value: i128, _size: u64) -> u128 {
        // TODO: find the rust docs on casting to prove this? I couldn't find it.
        // also remove this function once I prove it.
        value as u128
    }
}

// ----------------------------------------------- //

macro_rules! impl_from_uint {
    ($($ty:ty),*) => {
        $(
            impl From<$ty> for ConstValue {
                fn from(value: $ty) -> Self {
                    Self::Int {
                        bit_rep: Wrapping(value as u128),
                        ty: IntType {
                            bit_size: std::mem::size_of::<$ty>() as u64 * 8,
                            is_signed: false,
                        },
                    }
                }
            }
        )*
    };
}

impl_from_uint!(u8, u16, u32, u64, u128, usize);

impl From<char> for ConstValue {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}

// FIXME: Remove this error suppression after adding support for more variants
#[allow(unused)]
#[derive(Clone, Debug)]
pub(crate) enum AdtKind {
    Struct,
    Enum {
        /* NOTE: Even when the variant index is set based on a decision on a
         * symbolic value, the discriminant is still a concrete value and the
         * symbolic value will appear in the constraints.
         */
        discriminant: VariantIndex,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct AdtValue {
    pub kind: AdtKind,
    pub fields: Vec<Option<ValueRef>>,
}
impl AdtValue {
    /// creates an ADT that is the result of a checked operation that overflowed
    fn checked_overflow() -> Self {
        Self {
            kind: AdtKind::Struct,
            fields: vec![None, Some(ConstValue::Bool(true).to_value_ref())],
        }
    }

    /// creates an ADT that represents the result of a successful checked operation (no overflow)
    fn checked_success(result: u128, ty: IntType) -> Self {
        Self {
            kind: AdtKind::Struct,
            fields: vec![
                Some(
                    ConstValue::Int {
                        bit_rep: Wrapping(result), // TODO: figure out wrapping arithmetic
                        ty,
                    }
                    .to_value_ref(),
                ),
                Some(ConstValue::Bool(false).to_value_ref()),
            ],
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ArrayValue {
    pub elements: Vec<ValueRef>,
}

impl ArrayValue {
    #[inline]
    pub fn len(&self) -> usize {
        self.elements.len()
    }
}

#[derive(Clone, Debug)]
pub(crate) enum RefValue {
    /* NOTE:
     * Is it possible to omit Ref?
     * Immutable references can be directly represented as ValueRefs (with not recursive indirection).
     * Because while they are still alive, mutations are not possible and the same value
     * can be circulated. Also, when they are dead but used before in other
     * expressions, they will not be affected. So, basically they will exactly
     * like copy operands and the copy-on-write mechanism will guarantee that
     * they hold the correct value.
     * Also, from another point of view, ValueRef is a reference itself.
     *
     * Is it also possible to omit this MutRef?
     * It is, but it looks like it requires taking care of the lifetime of the
     * mutable reference. In that case we can move the value (instead of copy)
     * and then return to its place when the mutable reference is dropped. This
     * is because the actual place cannot be used while the mutable reference is
     * alive. So, it is basically a move. A note to mention here is that the
     * lifetime doesn't need to be handled explicitly, because on the first
     * usage of the original place, the mutable reference should be dead.
     */
    Immut(ValueRef),
    Mut(Place),
}

#[derive(Clone, Debug)]
pub(crate) enum SymValue {
    Variable(SymbolicVar),
    Expression(Expr),
}

#[derive(Clone, Debug)]
pub(crate) struct SymbolicVar {
    pub id: SymVarId,
    pub ty: ValueType,
}

impl SymbolicVar {
    pub fn new(id: SymVarId, ty: ValueType) -> Self {
        Self { id, ty }
    }
}

define_reversible_pair!(
    #[derive(Clone, Debug)]
    BinaryOperands<F, S> {
        (Orig, Rev) {
            first: F,
            second: S,
        }
    },
    #[allow(dead_code)]
    impl
);

type SymBinaryOperands = BinaryOperands<SymValueRef, ValueRef>;

// FIXME: Remove this error suppression after adding support for more variants
#[allow(unused)]
#[derive(Clone, Debug)]
pub(crate) enum Expr {
    Unary {
        operator: UnaryOp,
        operand: SymValueRef,
    },

    Binary {
        operator: BinaryOp,
        operands: SymBinaryOperands,
        checked: bool,
    },

    Cast {
        from: SymValueRef,
        to: ValueType,
    },

    AddrOf(/* TODO */),

    Len {
        of: SymValueRef,
    },

    Projection(ProjExpr),
}

// FIXME: Remove this error suppression after adding support for symbolic projection.
#[allow(unused)]
#[derive(Clone, Debug)]
pub(crate) enum ProjExpr {
    SymIndex {
        host: ConcreteValueRef,
        index: SymValueRef,
        from_end: bool,
    },
    SymHost {
        host: SymValueRef,
        kind: ProjKind,
    },
}

// FIXME: Remove this error suppression after adding support for symbolic projection.
#[allow(unused)]
#[derive(Clone, Debug)]
pub(crate) enum ProjKind {
    Deref,
    Field(FieldIndex),
    Index { index: ValueRef, from_end: bool },
    Subslice { from: u64, to: u64, from_end: bool },
    Downcast,
}

macro_rules! define_value_guard {
    ($guarded_type:ty, $name: ident, $pattern:pat, $value_name:ident) => {
        #[derive(Clone, Debug)]
        pub(crate) struct $name<V>(pub V);

        impl<V> $name<V>
        where
            V: AsRef<Value>,
        {
            pub fn new(value: V) -> Self {
                #![allow(unused_variables)]
                assert_matches!(
                    value.as_ref(),
                    $pattern,
                    concat!("Value should be ", stringify!($guarded_type), ".")
                );
                Self(value)
            }
        }

        impl<V> Deref for $name<V>
        where
            V: AsRef<Value>,
        {
            type Target = $guarded_type;

            fn deref(&self) -> &Self::Target {
                match self.0.as_ref() {
                    $pattern => $value_name,
                    _ => unreachable!(),
                }
            }
        }

        impl<V> AsRef<$guarded_type> for $name<V>
        where
            Self: Deref<Target = $guarded_type>,
        {
            fn as_ref(&self) -> &$guarded_type {
                self
            }
        }

        impl From<$name<ValueRef>> for ValueRef {
            fn from(value: $name<ValueRef>) -> Self {
                value.0
            }
        }

        impl<'a> $name<&'a mut ValueRef> {
            pub fn make_mut(self) -> &'a mut $guarded_type {
                match ValueRef::make_mut(self.0) {
                    $pattern => $value_name,
                    _ => unreachable!(),
                }
            }
        }
    };
}

define_value_guard!(
    ConcreteValue,
    ConcreteValueGuard,
    Value::Concrete(value),
    value
);
define_value_guard!(SymValue, SymValueGuard, Value::Symbolic(value), value);

define_reversible_pair!(
    SymIndexPair {
        (SymHost, SymIndex) {
            host: SymValueRef,
            index: ValueRef,
        }
    },
    #[allow(dead_code)]
    impl
);

mod convert {
    use super::*;
    use crate::backends::basic::operand;

    impl Value {
        #[inline]
        pub(crate) fn to_value_ref(self) -> ValueRef {
            ValueRef::new(self)
        }
    }

    impl From<ConcreteValue> for Value {
        #[inline]
        fn from(val: ConcreteValue) -> Self {
            Value::Concrete(val)
        }
    }

    impl ConstValue {
        #[inline]
        pub(crate) fn to_value_ref(self) -> ValueRef {
            Into::<Value>::into(self).to_value_ref()
        }
    }
    impl From<ConstValue> for ConcreteValue {
        #[inline]
        fn from(val: ConstValue) -> Self {
            ConcreteValue::Const(val)
        }
    }
    impl From<ConstValue> for Value {
        #[inline]
        fn from(val: ConstValue) -> Self {
            Into::<ConcreteValue>::into(val).into()
        }
    }
    impl From<operand::Constant> for ConstValue {
        #[inline]
        fn from(val: operand::Constant) -> Self {
            match val {
                operand::Constant::Bool(value) => ConstValue::Bool(value),
                operand::Constant::Char(value) => ConstValue::Char(value),
                operand::Constant::Int { bit_rep, ty } => ConstValue::Int {
                    bit_rep: Wrapping(bit_rep),
                    ty,
                },
                operand::Constant::Float { bit_rep, ty } => ConstValue::Float { bit_rep, ty },
                operand::Constant::Str(value) => ConstValue::Str(value),
                operand::Constant::Func(value) => ConstValue::Func(value),
            }
        }
    }

    impl SymValue {
        #[inline]
        pub fn to_value_ref(self) -> SymValueRef {
            SymValueGuard::new(ValueRef::new(self.into()))
        }
    }
    impl From<SymValue> for Value {
        #[inline]
        fn from(value: SymValue) -> Self {
            Value::Symbolic(value)
        }
    }

    impl Expr {
        #[inline]
        pub fn to_value_ref(self) -> SymValueRef {
            Into::<SymValue>::into(self).to_value_ref()
        }
    }
    impl From<Expr> for SymValue {
        #[inline]
        fn from(val: Expr) -> Self {
            SymValue::Expression(val)
        }
    }
    impl From<Expr> for Value {
        #[inline]
        fn from(val: Expr) -> Self {
            Into::<SymValue>::into(val).into()
        }
    }

    impl ProjExpr {
        #[inline]
        pub fn to_value_ref(self) -> SymValueRef {
            Into::<SymValue>::into(self).to_value_ref()
        }
    }
    impl From<ProjExpr> for Expr {
        #[inline]
        fn from(val: ProjExpr) -> Self {
            Expr::Projection(val)
        }
    }
    impl From<ProjExpr> for SymValue {
        #[inline]
        fn from(val: ProjExpr) -> Self {
            Into::<Expr>::into(val).into()
        }
    }
}
