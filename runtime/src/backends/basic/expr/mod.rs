use std::rc::Rc;

use crate::abs::{BinaryOp, FieldIndex, UnaryOp, VariantIndex};

use super::place::Place;

pub(super) type ValueRef = Rc<Value>;
pub(super) type SymValueRef = Rc<SymValue>;

#[derive(Clone, Debug)]
pub(super) enum Value {
    Concrete(ConcreteValue),
    Symbolic(SymValue),
}

impl Value {
    pub(super) fn is_symbolic(&self) -> bool {
        matches!(self, Value::Symbolic(_))
    }
}

#[derive(Clone, Debug)]
pub(super) enum ConcreteValue {
    Const(ConstValue),
    Adt(AdtValue),
    Array { len: u64, elements: Vec<ValueRef> },
    Ref(RefValue),
}

impl ConcreteValue {
    pub fn try_to_adt(&mut self) -> Option<&mut AdtValue> {
        match self {
            Self::Adt(value) => Some(value),
            _ => None,
        }
    }

    pub fn try_to_array(&mut self) -> Option<&mut Vec<ValueRef>> {
        match self {
            Self::Array { elements, .. } => Some(elements),
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

impl From<usize> for ConstValue {
    fn from(value: usize) -> Self {
        Self::Int {
            bit_rep: value as u128,
            size: std::mem::size_of::<usize>() as u64 * 8,
            is_signed: false,
        }
    }
}

#[derive(Clone, Debug)]
pub(super) enum AdtKind {
    Struct,
    Enum(VariantIndex),
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
        op: UnaryOp,
        expr: SymValueRef,
    },
    Binary {
        op: BinaryOp,
        left: SymValueRef,
        right: ValueRef,
    },

    Cast(/* TODO */),

    AddrOf(/* TODO */),
    Deref(SymValueRef),

    Discriminant {
        of: SymValueRef,
    },

    ArrayInit(ArrayInitExpr),
    Index {
        on: ValueRef,
        index: ValueRef,
        from_end: bool,
    },
    Slice {
        of: ValueRef,
        from: ValueRef,
        to: ValueRef,
        from_end: bool,
    },
    Len(SymValueRef),
}

#[derive(Clone, Debug)]
pub(super) enum ArrayInitExpr {
    Repeat { element: ValueRef, count: usize },
    Elements { elements: Vec<ValueRef> },
}
