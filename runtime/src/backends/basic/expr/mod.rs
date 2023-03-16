use std::{ops::Deref, rc::Rc};

use crate::abs::{BinaryOp, FieldIndex, UnaryOp, VariantIndex};

use super::place::Place;

pub(super) type ValueRef = Rc<Value>;
pub(super) type SymValueRef = SymValueGuard;

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
                Self::Float {
                    bit_rep,
                    ebits,
                    sbits,
                } => unimplemented!(),
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
            BinaryOp::Add => match (first, second) {
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
                    assert!(
                        first_size == second_size,
                        "Addition of integers with different sizes is not expected."
                    );
                    assert!(
                        first_signed == second_signed,
                        "Addition of integers with different signed modes is not expected."
                    );

                    Self::Int {
                        bit_rep: first + second,
                        size: *first_size,
                        is_signed: *first_signed,
                    }
                }
                (
                    Self::Float {
                        bit_rep: first,
                        ebits: first_ebits,
                        sbits: first_sbits,
                    },
                    Self::Float {
                        bit_rep: second,
                        ebits: second_ebits,
                        sbits: second_sbits,
                    },
                ) => unimplemented!(),
                _ => unreachable!("Addition only works on integers."),
            },
            _ => unimplemented!(),
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

#[derive(Clone, Copy, Debug)]
pub(super) struct SymbolicVar {
    id: u64,
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

    Cast(/* TODO */),

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

#[derive(Clone, Debug)]
pub(super) struct SymValueGuard(ValueRef);

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
