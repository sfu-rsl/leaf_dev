pub(super) mod builders;
mod fmt;
pub(crate) mod prelude;
pub(super) mod proj;
pub(super) mod sym_place;
pub(super) mod translators;

use std::{assert_matches::assert_matches, num::Wrapping, ops::Deref, rc::Rc};

use derive_more as dm;

use crate::abs::{
    BinaryOp, FieldIndex, FloatType, FuncId, IntType, PointerOffset, RawPointer, TypeId, UnaryOp,
    ValueType, VariantIndex,
};

use crate::utils::meta::define_reversible_pair;

use super::FullPlace;

pub(crate) type ValueRef = Rc<Value>;
pub(crate) type ConcreteValueRef = ConcreteValueGuard<ValueRef>;
pub(crate) type ConcreteValueMutRef<'a> = ConcreteValueGuard<&'a mut ValueRef>;
pub(crate) type SymValueRef = SymValueGuard<ValueRef>;
pub(crate) type ProjExprRef = ProjExprGuard<ValueRef>;

pub(crate) type SymVarId = u32;

#[derive(Clone, Debug, dm::From)]
pub(crate) enum Value {
    #[from(types(ConstValue, UnevalValue))]
    Concrete(ConcreteValue),
    #[from(types(Expr, ProjExpr))]
    Symbolic(SymValue),
}

impl Value {
    pub(crate) fn is_symbolic(&self) -> bool {
        matches!(self, Value::Symbolic(_))
    }
}

#[derive(Clone, Debug, dm::From)]
pub(crate) enum ConcreteValue {
    #[from]
    Const(ConstValue),
    #[from]
    Adt(AdtValue),
    #[from]
    Array(ArrayValue),
    #[from]
    Ref(RefValue),
    #[from(types(RawConcreteValue, PorterValue))]
    Unevaluated(UnevalValue),
}

// FIXME: Remove this error suppression after adding support for floats.
#[allow(unused)]
#[derive(Clone, Debug, Eq, PartialEq, dm::From)]
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
    #[from(ignore)]
    Func(FuncId),
    Zst,
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
                    bit_rep: !bit_rep + Wrapping(1),
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
                    ConcreteValue::Adt(Self::binary_op_checked_arithmetic(first, second, operator))
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
             * most 128 bits so we can just truncate the leading bits to get the correct bit representation.
             */
            Self::Int { bit_rep, .. } => Self::Int {
                bit_rep: {
                    // truncate leading bits as necessary
                    let result = Wrapping(Self::to_size((*bit_rep).0, &to));
                    debug_assert!(Self::in_bounds(result.0, &to), "result out of bounds");
                    result
                },
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
            first: &Wrapping<u128>,
            second: &Wrapping<u128>,
            ty @ IntType { is_signed, .. }: IntType, // pattern matching in function args, cool!
        ) -> Option<u128> {
            // we don't want any wrapping in this function, so we take the 0th parameter
            let (first, second) = (first.0, second.0);
            if is_signed {
                // casting between same sized integers is a no-op (see rust docs)
                let first = first as i128;
                let second = second as i128;

                let result = match operator {
                    BinaryOp::Add => first.checked_add(second),
                    BinaryOp::Sub => first.checked_sub(second),
                    BinaryOp::Mul => first.checked_mul(second),
                    _ => unreachable!("unsupported by rust"),
                };

                if let Some(result) = result {
                    // case: i128 has not overflowed, so result is valid. Ensure we're in our type's bounds
                    if ConstValue::in_bounds(result as u128, &ty) {
                        Some(result as u128)
                    } else {
                        None
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

                // If u128 overflows, any smaller type also overflows.
                result.filter(|result| {
                    // u128 has not overflowed, check if we're higher than our max.
                    ConstValue::in_bounds(*result, &ty)
                })
            }
        }

        // only integers are supported, as per the rust docs:
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
                    ty: ty_second @ IntType { .. },
                },
            ) => {
                assert_eq!(*ty, *ty_second);

                let result = checked_op(operator, first, second, *ty);
                match result {
                    Some(result) => {
                        let ty = IntType {
                            bit_size: *first_size,
                            is_signed: *first_signed,
                        };
                        AdtValue::checked_success(Wrapping(result), ty)
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
                assert!(
                    !second_ty.is_signed || Self::is_positive(second.0, second_ty.bit_size),
                    "Shifting by a negative value is not expected."
                ); //TODO we can get rid of this assertion in the future

                let result = match operator {
                    // if second.0 is u128 or u64 & too big for usize, it will be usize::MAX
                    BinaryOp::Shl => first << second.0 as usize,
                    BinaryOp::Shr => first >> second.0 as usize,
                    _ => unreachable!("invalid binop"),
                };

                let result = Wrapping(Self::to_size(result.0, first_ty));

                debug_assert!(
                    Self::in_bounds(result.0, first_ty),
                    "result {} out of bounds",
                    result.0
                );

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

                    if first_ty.is_signed {
                        // `as i128` is a bitwise transmute
                        let first = first.0 as i128;
                        let second = second.0 as i128;
                        match operator {
                            BinaryOp::Lt => first < second,
                            BinaryOp::Le => first <= second,
                            BinaryOp::Ge => first >= second,
                            BinaryOp::Gt => first > second,
                            _ => unreachable!(),
                        }
                    } else {
                        match operator {
                            BinaryOp::Lt => first < second,
                            BinaryOp::Le => first <= second,
                            BinaryOp::Ge => first >= second,
                            BinaryOp::Gt => first > second,
                            _ => unreachable!(),
                        }
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

    /// determines whether value is valid for the type ty (not left zero-padded, for example)
    fn in_bounds(value: u128, ty: &IntType) -> bool {
        if ty.is_signed {
            let max = ((1_u128 << (ty.bit_size - 1)) - 1) as i128;
            let min = match ty.bit_size {
                0..=127 => -(1_i128 << (ty.bit_size - 1)),
                128 => i128::MIN,
                129..=u64::MAX => panic!("unsupported integer size; too large"),
            };
            let value = value as i128;
            value <= max && value >= min
        } else {
            let max = match ty.bit_size {
                0..=127 => (1_u128 << ty.bit_size) - 1,
                128 => u128::MAX,
                129..=u64::MAX => panic!("unsupported integer size; too large"),
            };
            value <= max
        }
    }

    /// Applies truncation and casting as necessary to keep value within ty's size bounds
    /// (despite still being a u128 value)
    fn to_size(value: u128, ty: &IntType) -> u128 {
        if ty.bit_size == 128 {
            return value;
        }

        // create a mask of all the significant bits, then truncate to size
        let mask: u128 = (1_u128 << (ty.bit_size as u128)) - 1;
        let value = value & mask;

        if ty.is_signed {
            Self::sign_ext(value, ty.bit_size)
        } else {
            value // implicit zero extension
        }
    }

    /// A sign extension is equivalent to casting value to ty, then to u128
    fn sign_ext(value: u128, bit_size: u64) -> u128 {
        let bits_to_shift = 128 - bit_size;
        let value = value as i128;
        ((value << bits_to_shift) >> bits_to_shift) as u128
    }
}

// ----------------------------------------------- //

// FIXME: Remove this error suppression after adding support for more variants
#[allow(unused)]
#[derive(Clone, Debug)]
pub(crate) enum AdtKind {
    Tuple,
    Struct,
    Enum {
        /* NOTE: Even when the variant index is set based on a decision on a
         * symbolic value, its value is still a concrete and the symbolic value
         * will affect the constraints. */
        variant: VariantIndex,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct AdtValue {
    pub kind: AdtKind,
    pub fields: Vec<AdtField>,
}
impl AdtValue {
    /// creates an ADT that is the result of a checked operation that overflowed
    fn checked_overflow() -> Self {
        Self {
            kind: AdtKind::Struct,
            fields: vec![
                Self::field_for_checked(None),
                Self::field_for_checked(Some(true.into())),
            ],
        }
    }

    /// creates an ADT that represents the result of a successful checked operation (no overflow)
    fn checked_success(result: Wrapping<u128>, ty: IntType) -> Self {
        Self {
            kind: AdtKind::Struct,
            fields: vec![
                Self::field_for_checked(Some(ConstValue::Int {
                    bit_rep: result,
                    ty,
                })),
                Self::field_for_checked(Some(false.into())),
            ],
        }
    }

    fn field_for_checked(value: Option<ConstValue>) -> AdtField {
        AdtField {
            value: value.map(ConstValue::to_value_ref),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct AdtField {
    pub value: Option<ValueRef>,
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
     * Immutable references can be directly represented as ValueRefs (with no recursive indirection).
     * Because as long as they are alive, mutations are not possible and the same value
     * can be circulated.
     * So, basically they will act like copied values and the copy-on-write mechanism (in Rc)
     * guarantees that value is not changed.
     * Also, from another point of view, ValueRef is a reference itself so should fit well
     * in a reference representation.
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
    Mut(FullPlace),
}

#[derive(Clone, Debug, dm::From)]
pub(crate) enum UnevalValue {
    Some,
    Lazy(RawConcreteValue),
    Porter(PorterValue),
}

#[derive(Clone, Debug)]
pub(crate) struct RawConcreteValue(pub(crate) RawPointer, pub(crate) Option<ValueType>);

impl RawConcreteValue {
    pub(crate) unsafe fn evaluate(&self) -> ConcreteValue {
        let Some(ty) = &self.1 else {
            panic!("The type for the lazy value is not available.");
        };
        /* NOTE: Can we perform evaluation without storing the type separately?
         * It seems to be possible as we evaluate them only when we
         * are evaluating a operation which includes a symbolic value (otherwise
         * the concrete result would be referenced).
         * Thus, we should be able to infer the type based on the type of symbolic
         * value.
         * However there are a few cases that this is not easily achievable:
         * - In shift operations, which the operands may not be from the same type.
         * - Arrays and effectively symbolic projections.
         * So we may end up with a hybrid solution. Currently, we have defined
         * the type as an option to not pass the type in the future unless it is
         * necessary.
         */
        let addr = self.0 as usize;
        use std::ptr::from_exposed_addr as to_ptr;
        let value: ConstValue = match ty {
            ValueType::Bool => (*(to_ptr::<bool>(addr))).into(),
            ValueType::Char => (*(to_ptr::<char>(addr))).into(),
            ValueType::Int(ty @ IntType { bit_size, .. }) => ConstValue::Int {
                bit_rep: Wrapping(Self::read_int(addr, *bit_size as usize)),
                ty: *ty,
            },
            ValueType::Float(_) => unimplemented!(),
        };
        value.into()
    }

    unsafe fn read_int(addr: usize, bit_size: usize) -> u128 {
        let bytes =
            std::slice::from_raw_parts(std::ptr::from_exposed_addr::<u8>(addr), bit_size / 8);

        #[cfg(target_endian = "big")]
        let bytes = bytes.iter();
        #[cfg(target_endian = "little")]
        let bytes = bytes.iter().rev();

        let mut result: u128 = 0;
        for byte in bytes {
            result = result << 8 | *byte as u128;
        }
        result
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PorterValue {
    pub(crate) sym_values: Vec<(PointerOffset, TypeId, SymValueRef)>,
}

#[derive(Clone, Debug, dm::From)]
pub(crate) enum SymValue {
    Variable(SymbolicVar),
    #[from(types(BinaryExpr, ProjExpr))]
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
#[derive(Clone, Debug, dm::From)]
pub(crate) enum Expr {
    Unary {
        operator: UnaryOp,
        operand: SymValueRef,
    },

    Binary(BinaryExpr),

    Cast {
        from: SymValueRef,
        to: ValueType,
    },

    AddrOf(ProjExprRef),

    #[from(ignore)]
    Len(ProjExprRef),

    Projection(ProjExpr),
}

#[allow(unused)]
#[derive(Clone, Debug, dm::From)]
pub(crate) struct BinaryExpr<Operands = SymBinaryOperands> {
    operator: BinaryOp,
    operands: Operands,
    checked: bool,
}

// FIXME: Remove this error suppression after adding support for symbolic projection.
#[allow(unused)]
#[derive(Clone, Debug)]
pub(crate) enum ProjExpr {
    SymIndex(ConcreteHostProj),
    SymHost(SymHostProj),
}

#[derive(Clone, Debug)]
pub(crate) struct ConcreteHostProj {
    host: ConcreteValueRef,
    index: SliceIndex<SymValueRef>,
}

#[allow(unused)]
#[derive(Clone, Debug)]
pub(crate) struct SymHostProj {
    host: SymValueRef,
    kind: ProjKind,
}

// FIXME: Remove this error suppression after adding support for symbolic projection.
#[allow(unused)]
#[derive(Clone, Debug)]
pub(crate) enum ProjKind {
    Deref,
    Field(FieldIndex),
    Index(SliceIndex<ValueRef>),
    Subslice { from: u64, to: u64, from_end: bool },
    Downcast(DowncastKind),
}

#[derive(Clone, Debug)]
pub(crate) struct SliceIndex<I> {
    pub index: I,
    pub from_end: bool,
}

/* NOTE: Why is transmutation modeled as a projection?
 * There is a set of options for modeling transmutation.
 * 1. A variant of `Expr` or `CastExpr`:
 * This is probably the most natural way. However, it requires loosening the
 * constraints based on the fact that only `ProjExpr` can be from any type and
 * the rest are from primitive types.
 * 2. A variant of `ProjKind`:
 * This is the current approach. As it does not change the underlying data, but
 * what parts of it will be read (like `u64` to [u8; 8]), it looks like a
 * projection. Additionally, it can be viewed as a similar projection like
 * `Downcast` in the same sense that no data is changed but the layout is
 * reinterpreted. It keeps the fact about `ProjExpr` types.
 * However, it does not follow MIR modeling. More importantly, it breaks
 * the assumption that projection expressions only start with a symbolic index
 * and the result of symbolic projections are multiple possible values.
 * 3. Resolving at the builder level:
 * This could make the translation easier. However, in addition to changing the
 * facts like 1, it would be inefficient as it requires eager calculation.
 * For example, a cast between a large array to another can be easily skipped by
 * the others, but here it should be represented eagerly.
 */
#[derive(Clone, Copy, Debug, dm::From)]
pub(crate) enum DowncastKind {
    EnumVariant(VariantIndex),
    Transmutation(TypeId),
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
            #[allow(unused)]
            pub fn make_mut(self) -> &'a mut $guarded_type {
                match ValueRef::make_mut(self.0) {
                    $pattern => $value_name,
                    _ => unreachable!(),
                }
            }
        }

        impl<V: Clone> $name<V> {
            pub fn clone_to(&self) -> V {
                self.0.clone()
            }
        }

        impl<V> std::fmt::Display for $name<V>
        where
            V: std::fmt::Display,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt(f)
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
define_value_guard!(
    ProjExpr,
    ProjExprGuard,
    Value::Symbolic(SymValue::Expression(Expr::Projection(proj))),
    proj
);

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

#[allow(clippy::wrong_self_convention)]
mod convert {
    use super::*;
    use crate::abs;

    impl Value {
        #[inline]
        pub(crate) fn to_value_ref(self) -> ValueRef {
            ValueRef::new(self)
        }

        pub(crate) fn unwrap_func_id(&self) -> u64 {
            if let Value::Concrete(ConcreteValue::Const(ConstValue::Func(f))) = *self {
                f
            } else {
                panic!("func must be ConstValue::Func(f)")
            }
        }
    }

    impl ConcreteValue {
        #[inline]
        pub(crate) fn to_value_ref(self) -> ValueRef {
            Into::<Value>::into(self).to_value_ref()
        }
    }
    impl From<abs::Constant> for ConcreteValue {
        #[inline]
        fn from(val: abs::Constant) -> Self {
            use abs::Constant::*;
            match val {
                ByteStr(bytes) => Self::Ref(RefValue::Immut(
                    Self::Array(ArrayValue {
                        elements: bytes
                            .iter()
                            .copied()
                            .map(ConstValue::from)
                            .map(ConstValue::to_value_ref)
                            .collect(),
                    })
                    .to_value_ref(),
                )),
                #[cfg(abs_concrete)]
                Some => UnevalValue::Some.into(),
                _ => Self::Const(match val {
                    Bool(value) => value.into(),
                    Char(value) => value.into(),
                    Int { bit_rep, ty } => ConstValue::Int {
                        bit_rep: Wrapping(bit_rep),
                        ty,
                    },
                    Float { bit_rep, ty } => ConstValue::Float { bit_rep, ty },
                    Str(value) => ConstValue::Str(value),
                    Func(value) => ConstValue::Func(value),
                    Zst => ConstValue::Zst,
                    ByteStr(_) => unreachable!(),
                    #[cfg(abs_concrete)]
                    Some => unreachable!(),
                }),
            }
        }
    }

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

    macro_rules! impl_conc_to_value_ref {
        ($($ty: ty),*) => {
            $(
                impl $ty {
                    #[inline]
                    pub(crate) fn to_value_ref(self) -> ValueRef {
                        Into::<ConcreteValue>::into(self).to_value_ref()
                    }
                }
            )*
        };
    }

    impl_conc_to_value_ref!(ConstValue, UnevalValue, RawConcreteValue, PorterValue);

    impl SymValue {
        #[inline]
        pub fn to_value_ref(self) -> SymValueRef {
            SymValueRef::new(ValueRef::new(self.into()))
        }
    }

    impl Expr {
        #[inline]
        pub fn to_value_ref(self) -> SymValueRef {
            Into::<SymValue>::into(self).to_value_ref()
        }
    }

    impl BinaryExpr {
        #[inline]
        pub fn to_value_ref(self) -> SymValueRef {
            Into::<SymValue>::into(self).to_value_ref()
        }
    }

    impl ProjExpr {
        #[inline]
        pub fn to_value_ref(self) -> SymValueRef {
            Into::<SymValue>::into(self).to_value_ref()
        }
    }
}
