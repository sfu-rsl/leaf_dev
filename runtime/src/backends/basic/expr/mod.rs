pub(super) mod translators;

use std::{ops::Deref, rc::Rc};

use super::abs::{BinaryOp, FieldIndex, UnaryOp, VariantIndex};

use super::{operand, place::Place};

pub(super) type ValueRef = Rc<Value>;
pub(super) type SymValueRef = SymValueGuard;
pub(super) type SymVarId = u32;

#[derive(Clone, Debug)]
pub(super) enum Value {
    Concrete(ConcreteValue),
    Symbolic(SymValue),
}

impl Value {
    pub fn from_const(value: ConstValue) -> Self {
        Self::Concrete(ConcreteValue::from_const(value))
    }

    pub(super) fn is_symbolic(&self) -> bool {
        matches!(self, Value::Symbolic(_))
    }
}

#[derive(Clone, Debug)]
pub(super) enum ConcreteValue {
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

impl ConcreteValue {
    pub fn try_to_adt(&mut self) -> Option<&mut AdtValue> {
        match self {
            Self::Adt(value) => Some(value),
            _ => None,
        }
    }

    pub fn try_to_ref(&self) -> Option<&RefValue> {
        match self {
            Self::Ref(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(super) enum ConstValue {
    Bool(bool),
    Char(char),
    Int {
        bit_rep: u128,
        size: u64,
        is_signed: bool,
    },
    Float {
        bit_rep: u128,
        ebits: u64,
        sbits: u64,
    },
    Str(&'static str),
    Func(u64),
}

pub fn assert_size_equality(first_size: &u64, second_size: &u64) {
    assert!(
        first_size == second_size,
        "Size equality assertion on integers failed, this is regarding binary_op function for constants."
    );
}

pub fn assert_sign_equality(first_signed: &bool, second_signed: &bool) {
    assert!(
        first_signed == second_signed,
        "Sign equality assertion on integers failed, this is regarding binary_op function for constants."
    );
}

pub fn are_positive(first: &u128, second: &u128) -> (bool, bool) {
    let mut first_positive: bool = true;
    let mut second_positive: bool = true;
    let mask: u128 = 1;
    if (first >> 127) & mask == 1 {
        first_positive = false;
    }

    if (second >> 127) & mask == 1 {
        second_positive = false;
    }

    return (first_positive, second_positive);
}

impl ConstValue {
    pub fn unary_op(this: &Self, operator: UnaryOp) -> ConstValue {
        match operator {
            UnaryOp::Neg => match this {
                Self::Int {
                    bit_rep,
                    size,
                    is_signed: true,
                } => Self::Int {
                    bit_rep: todo!("Proposed value: {}", !bit_rep + 1),
                    size: *size,
                    is_signed: true,
                },
                Self::Float { .. } => unimplemented!(),
                _ => unreachable!("Negation of non-numeric constant is not possible."),
            },
            UnaryOp::Not => match this {
                Self::Bool(value) => Self::Bool(!value),
                Self::Int {
                    bit_rep,
                    size,
                    is_signed,
                } => Self::Int {
                    bit_rep: !bit_rep,
                    size: *size,
                    is_signed: *is_signed,
                },
                _ => unreachable!("Not operand only works on boolean and integers."),
            },
        }
    }

    pub fn binary_op(first: &Self, second: &Self, operator: BinaryOp) -> ConstValue {
        match operator {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::BitXor
            | BinaryOp::BitAnd
            | BinaryOp::BitOr => match (first, second) {
                (
                    Self::Int {
                        bit_rep: first,
                        size: first_size,
                        is_signed: first_signed,
                    },
                    Self::Int {
                        bit_rep: second,
                        size: second_size,
                        is_signed: second_signed,
                    },
                ) => {
                    assert_size_equality(first_size, second_size);

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
                        size: *first_size,
                        is_signed: *first_signed,
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
            },

            BinaryOp::Shl | BinaryOp::Shr => {
                match (first, second) {
                    (
                        Self::Int {
                            bit_rep: first,
                            size: first_size,
                            is_signed: first_signed,
                        },
                        Self::Int {
                            bit_rep: second,
                            size: second_size,
                            is_signed: second_signed,
                        },
                    ) => {
                        let result = match operator {
                            BinaryOp::Shl => {
                                assert!(
                                    !*second_signed,
                                    "Shifting a negative value is not expected."
                                ); //TODO we can get rid of this assertion in the future
                                first << second
                            }
                            BinaryOp::Shr => {
                                assert!(
                                    !*second_signed,
                                    "Shifting by a negative value is not expected."
                                ); //TODO we can get rid of this assertion in the future
                                first >> second
                            }

                            _ => unreachable!(),
                        };

                        Self::Int {
                            bit_rep: result,
                            size: *first_size,
                            is_signed: *first_signed,
                        }
                    }

                    (Self::Float { .. }, Self::Float { .. }) => unimplemented!(),

                    (Self::Bool { .. }, Self::Bool { .. }) => unimplemented!(),

                    _ => unimplemented!(),
                }
            }

            BinaryOp::Eq
            | BinaryOp::Lt
            | BinaryOp::Le
            | BinaryOp::Ne
            | BinaryOp::Ge
            | BinaryOp::Gt => match (first, second) {
                (
                    Self::Int {
                        bit_rep: first,
                        size: first_size,
                        is_signed: first_signed,
                    },
                    Self::Int {
                        bit_rep: second,
                        size: second_size,
                        is_signed: second_signed,
                    },
                ) => {
                    assert_size_equality(first_size, second_size);
                    assert_sign_equality(first_signed, second_signed);

                    let signs = are_positive(first, second);

                    let result: bool = match signs {
                        (true, true) | (false, false) => match operator {
                            BinaryOp::Eq => (first == second),
                            BinaryOp::Lt => (first < second),
                            BinaryOp::Le => (first <= second),
                            BinaryOp::Ne => (first != second),
                            BinaryOp::Ge => (first >= second),
                            BinaryOp::Gt => (first > second),
                            _ => unreachable!(),
                        },
                        (false, true) => match operator {
                            BinaryOp::Eq => false,
                            BinaryOp::Lt => true,
                            BinaryOp::Le => true,
                            BinaryOp::Ne => true,
                            BinaryOp::Ge => false,
                            BinaryOp::Gt => false,
                            _ => unreachable!(),
                        },
                        (true, false) => match operator {
                            BinaryOp::Eq => false,
                            BinaryOp::Lt => false,
                            BinaryOp::Le => false,
                            BinaryOp::Ne => true,
                            BinaryOp::Ge => true,
                            BinaryOp::Gt => true,
                            _ => unreachable!(),
                        },

                        _ => unreachable!(),
                    };

                    Self::Bool(result)
                }

                (Self::Float { .. }, Self::Float { .. }) => unimplemented!(),

                (Self::Bool(first_value), Self::Bool(second_value)) => {
                    let result = match operator {
                        BinaryOp::Eq => first_value == second_value,
                        BinaryOp::Ne => first_value != second_value,
                        _ => unreachable!(),
                    };
                    Self::Bool(result)
                }

                _ => unimplemented!(),
            },

            _ => unimplemented!("{:?} {:?} {:?}", first, second, operator),
        }
    }

    pub fn integer_cast(this: &Self, to_size: u64, is_to_signed: bool) -> ConstValue {
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
                size: to_size,
                is_signed: is_to_signed,
            },
            Self::Bool(value) => Self::Int {
                bit_rep: *value as u128,
                size: to_size,
                is_signed: is_to_signed,
            },
            Self::Char(value) => Self::Int {
                bit_rep: *value as u128,
                size: to_size,
                is_signed: is_to_signed,
            },
            Self::Float { .. } => todo!("Casting float to integer is not implemented yet."),
            _ => unreachable!("Casting {this:?} to integer is not possible."),
        }
    }
}

macro_rules! impl_from_uint {
    ($($ty:ty),*) => {
        $(
            impl From<$ty> for ConstValue {
                fn from(value: $ty) -> Self {
                    Self::Int {
                        bit_rep: value as u128,
                        size: std::mem::size_of::<$ty>() as u64 * 8,
                        is_signed: false,
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

#[derive(Clone, Debug)]
pub(super) enum AdtKind {
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
pub(super) struct AdtValue {
    pub kind: AdtKind,
    pub fields: Vec<Option<ValueRef>>,
}

impl AdtValue {
    pub fn replace_field(&mut self, field: FieldIndex, replace: impl FnOnce(ValueRef) -> ValueRef) {
        let field = field as usize;
        let value = self.fields[field]
            .take()
            .expect("Field value is taken out before.");
        self.fields[field] = Some(replace(value));
    }
}

#[derive(Clone, Debug)]
pub(super) struct ArrayValue {
    pub elements: Vec<ValueRef>,
}

impl ArrayValue {
    #[inline]
    pub fn len(&self) -> usize {
        self.elements.len()
    }
}

#[derive(Clone, Debug)]
pub(super) enum RefValue {
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

impl RefValue {
    pub fn try_get_mut_place(&self) -> Option<&Place> {
        match self {
            Self::Mut(place) => Some(place),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub(super) enum SymValue {
    Variable(SymbolicVar),
    Expression(Expr),
}

impl SymValue {
    #[inline]
    pub fn into_value(self) -> Value {
        Value::Symbolic(self)
    }

    #[inline]
    pub fn into_value_ref(self) -> SymValueRef {
        SymValueGuard::new(ValueRef::new(self.into_value()))
    }
}

#[derive(Clone, Copy, Debug)]
pub(super) struct SymbolicVar {
    pub id: SymVarId,
    pub ty: SymbolicVarType,
}

impl SymbolicVar {
    pub fn new(id: SymVarId, ty: SymbolicVarType) -> Self {
        Self { id, ty }
    }
}

#[derive(Clone, Copy, Debug)]
pub(super) enum SymbolicVarType {
    Bool,
    Char,
    Int { size: u64, is_signed: bool },
    Float { ebits: u64, sbits: u64 },
}

impl From<&operand::Symbolic> for SymbolicVarType {
    fn from(value: &operand::Symbolic) -> Self {
        match value {
            operand::Symbolic::Bool => Self::Bool,
            operand::Symbolic::Char => Self::Char,
            operand::Symbolic::Int { size, is_signed } => Self::Int {
                size: *size,
                is_signed: *is_signed,
            },
            operand::Symbolic::Float { ebits, sbits } => Self::Float {
                ebits: *ebits,
                sbits: *sbits,
            },
        }
    }
}

#[derive(Clone, Debug)]
pub(super) enum Expr {
    Unary {
        operator: UnaryOp,
        operand: SymValueRef,
    },
    Binary {
        operator: BinaryOp,
        first: SymValueRef,
        second: ValueRef,
        is_flipped: bool,
    },

    Cast {
        from: SymValueRef,
        to: SymbolicVarType,
    },

    AddrOf(/* TODO */),
    Deref(SymValueRef),

    Index {
        on: ValueRef,
        index: SymValueRef,
        from_end: bool,
    },
    Slice {
        of: ValueRef,
        from: ValueRef,
        to: ValueRef,
        from_end: bool,
    },
}

impl Expr {
    #[inline]
    pub fn into_sym_value(self) -> SymValue {
        SymValue::Expression(self)
    }

    #[inline]
    pub fn into_value_ref(self) -> SymValueRef {
        self.into_sym_value().into_value_ref()
    }
}

#[derive(Clone, Debug)]
pub(super) struct SymValueGuard(pub ValueRef);

impl SymValueGuard {
    pub fn new(value: ValueRef) -> Self {
        assert!(value.is_symbolic(), "Value should be symbolic.");
        Self(value)
    }
}

impl Deref for SymValueGuard {
    type Target = SymValue;

    fn deref(&self) -> &Self::Target {
        match &*self.0 {
            Value::Symbolic(value) => value,
            _ => unreachable!(),
        }
    }
}
