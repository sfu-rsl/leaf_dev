pub(super) mod builders;
mod fmt;
pub(crate) mod lazy;
pub(super) mod place;
pub(crate) mod prelude;
pub(super) mod proj;
pub(super) mod sym_place;
pub(super) mod sym_placex;
pub(super) mod translators;

use std::{num::Wrapping, rc::Rc};

use derive_more as dm;

use common::tyexp::TypeInfo;

use crate::abs::expr::sym_place::{Select, SymbolicReadTree};
use crate::utils::meta::define_reversible_pair;
use place::SymPlaceValueRef;

pub(crate) use crate::abs::{
    self, FieldIndex, FloatType, FuncId, IntType, PointerOffset, RawAddress, TypeId, ValueType,
    VariantIndex,
};

pub(crate) type ValueRef = Rc<Value>;
pub(crate) type ConcreteValueRef = guards::ConcreteValueGuard<ValueRef>;
pub(crate) type SymValueRef = guards::SymValueGuard<ValueRef>;
pub(crate) type ProjExprRef = guards::ProjExprGuard<ValueRef>;

pub(crate) type SymVarId = u32;

#[derive(Clone, Debug, dm::From)]
pub(crate) enum Value {
    #[from(types(ConstValue, UnevalValue))]
    Concrete(ConcreteValue),
    #[from(types(Expr, ProjExpr))]
    Symbolic(SymValue),
}

impl Value {
    #[inline]
    pub(crate) fn is_symbolic(&self) -> bool {
        self.as_sym().is_some()
    }

    #[inline]
    pub(crate) fn as_sym(&self) -> Option<&SymValue> {
        match self {
            Value::Symbolic(sym) => Some(sym),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn as_proj(&self) -> Option<&ProjExpr> {
        self.as_sym().and_then(SymValue::as_proj)
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
    FatPointer(FatPtrValue),
    #[from(forward)]
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
    #[from(ignore)]
    Func(FuncId),
    #[from(ignore)]
    Addr(RawAddress),
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

    pub fn binary_op(first: &Self, second: &Self, operator: BinaryOp) -> ConcreteValue {
        match operator {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::BitXor
            | BinaryOp::BitAnd
            | BinaryOp::BitOr => {
                ConcreteValue::Const(Self::binary_op_arithmetic(first, second, operator))
            }
            BinaryOp::Shl | BinaryOp::Shr => {
                ConcreteValue::Const(Self::binary_op_shift(first, second, operator))
            }
            BinaryOp::AddWithOverflow | BinaryOp::SubWithOverflow | BinaryOp::MulWithOverflow => {
                ConcreteValue::Adt(Self::binary_op_with_overflow_arithmetic(
                    first, second, operator,
                ))
            }
            BinaryOp::AddUnchecked | BinaryOp::SubUnchecked | BinaryOp::MulUnchecked => {
                todo!("#197")
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

    fn binary_op_with_overflow_arithmetic(
        first: &Self,
        second: &Self,
        operator: BinaryOp,
    ) -> AdtValue {
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

#[derive(Clone, Debug, dm::From)]
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
pub(crate) struct FatPtrValue {
    pub address: ConcreteValueRef,
    pub metadata: ConcreteValueRef,
    pub ty: TypeId,
}

#[derive(Clone, Debug, dm::From)]
pub(crate) enum UnevalValue {
    Some,
    Lazy(RawConcreteValue),
    Porter(PorterValue),
}

#[derive(Clone, Debug)]
pub(crate) struct RawConcreteValue(
    pub(crate) RawAddress,
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
    pub(crate) Option<ValueType>,
    /* At the moment, type info is not supposed to be filled by default. */
    pub(crate) LazyTypeInfo,
);

#[derive(Clone, Debug)]
pub(crate) enum LazyTypeInfo {
    None,
    Id(TypeId),
    Fetched(&'static TypeInfo),
    Forced(Rc<TypeInfo>),
}

impl LazyTypeInfo {
    pub(crate) fn id(&self) -> Option<TypeId> {
        match self {
            Self::None => None,
            Self::Id(id) => Some(*id),
            Self::Fetched(ty) => Some(ty.id),
            Self::Forced(ty) => Some(ty.id),
        }
    }
}

impl From<Option<TypeId>> for LazyTypeInfo {
    fn from(ty_id: Option<TypeId>) -> Self {
        match ty_id {
            Some(ty_id) => Self::Id(ty_id),
            None => Self::None,
        }
    }
}

impl<'a> TryFrom<&'a TypeInfo> for ValueType {
    type Error = ();

    fn try_from(value: &'a TypeInfo) -> Result<Self, Self::Error> {
        use abs::USIZE_TYPE;
        // TODO: To be replaced with a well-cached implementation
        let name = value.name.as_str();
        match name {
            "bool" => Ok(ValueType::Bool),
            "char" => Ok(ValueType::Char),
            _ if name.starts_with('i') || name.starts_with('u') => name[1..]
                .parse()
                .map(|bit_size| {
                    IntType {
                        bit_size,
                        is_signed: name.starts_with('i'),
                    }
                    .into()
                })
                .or_else(|_| {
                    if name[1..] == *"size" {
                        Ok(IntType {
                            is_signed: name.starts_with('i'),
                            ..USIZE_TYPE
                        }
                        .into())
                    } else {
                        Err(())
                    }
                }),
            _ if name.starts_with("f") => unimplemented!(),
            "*mut ()" | "*const ()" => Ok(USIZE_TYPE.into()),
            _ => Err(()),
        }
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

impl SymValue {
    #[inline]
    pub(crate) fn as_proj(&self) -> Option<&ProjExpr> {
        match self {
            SymValue::Expression(Expr::Projection(host)) => Some(host),
            _ => None,
        }
    }
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

impl SymBinaryOperands {
    pub fn first(&self) -> &ValueRef {
        match self {
            Self::Orig { first, .. } => &first.0,
            Self::Rev { first, .. } => first,
        }
    }

    pub fn second(&self) -> &ValueRef {
        match self {
            Self::Orig { second, .. } => second,
            Self::Rev { second, .. } => &second.0,
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub(crate) enum UnaryOp {
    Neg = abs::UnaryOp::Neg as u8,
    Not = abs::UnaryOp::Not as u8,
}

pub(crate) type BinaryOp = crate::abs::BinaryOp;

// FIXME: Remove this error suppression after adding support for more variants
#[allow(unused)]
#[derive(Clone, Debug, dm::From)]
pub(crate) enum Expr {
    Unary {
        operator: UnaryOp,
        operand: SymValueRef,
    },

    Binary(BinaryExpr),

    Extension {
        source: SymValueRef,
        is_zero_ext: bool,
        bits_to_add: u32,
        /* NOTE: Currently, extension is only used for casting,
         * thus we include the destination type in the expression. */
        ty: ValueType,
    },

    Extraction {
        source: SymValueRef,
        high: u32,
        low: u32,
        /* NOTE: Currently, extraction is only used for casting,
         * thus we include the destination type in the expression. */
        ty: ValueType,
    },

    Ite {
        condition: SymValueRef,
        if_target: ValueRef,
        else_target: ValueRef,
    },

    Multi(MultiValue),

    /// A reference or the address of a value.
    /* NOTE: Should not we have a separate variant for the address of a value?
     * It should not be required in two ways:
     * First, the address of is obtained in the same way as a reference is obtained,
     * i.e., by taking a place. Also the case in which a reference is converted
     * to a pointer (address), it is first dereferenced and then the address is
     * obtained from the result. So effectively equivalent.
     * Second, we are working at runtime which means references are already
     * converted to pointers. So, we should also have the same behavior for both.
     */
    #[from(ignore)]
    Ref(SymPlaceValueRef),

    #[from(ignore)]
    Len(SymPlaceValueRef),

    Projection(ProjExpr),
}

#[allow(unused)]
#[derive(Clone, Debug, dm::From)]
pub(crate) struct BinaryExpr<Operands = SymBinaryOperands> {
    operator: BinaryOp,
    operands: Operands,
}

pub(crate) type SymIndex = SliceIndex<SymValueRef>;
pub(crate) type MultiValueLeaf = ValueRef;
pub(crate) type MultiValueTree = SymbolicReadTree<SymIndex, MultiValueLeaf>;
pub(crate) type MultiValueArray = Vec<MultiValueTree>;
pub(crate) type MultiValue = Select<SymIndex, MultiValueTree>;

// FIXME: Remove this error suppression after adding support for symbolic projection.
#[allow(unused)]
#[derive(Clone, Debug, dm::From)]
pub(crate) enum ProjExpr<M = ProjMetadata> {
    SymIndex(ConcreteHostProj<M>),
    SymHost(SymHostProj<M>),
}

impl<M> ProjExpr<M> {
    pub fn metadata(&self) -> &M {
        match self {
            ProjExpr::SymIndex(proj) => &proj.metadata,
            ProjExpr::SymHost(proj) => &proj.metadata,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ConcreteHostProj<M = ProjMetadata> {
    pub host: ConcreteValueRef,
    pub index: SliceIndex<SymValueRef>,
    pub metadata: M,
}

#[allow(unused)]
#[derive(Clone, Debug)]
pub(crate) struct SymHostProj<M = ProjMetadata> {
    pub host: SymValueRef,
    pub kind: ProjKind,
    pub metadata: M,
}

#[derive(Clone, Debug)]
pub(crate) struct ProjMetadata {
    host_type_id: Option<TypeId>,
}

impl ProjMetadata {
    pub fn new(host_type_id: TypeId) -> Self {
        Self {
            host_type_id: Some(host_type_id),
        }
    }

    pub fn unknown() -> Self {
        Self { host_type_id: None }
    }

    pub fn host_type_id(&self) -> TypeId {
        self.host_type_id.expect("Host type id is not set.")
    }
}

// FIXME: Remove this error suppression after adding support for symbolic projection.
#[allow(unused)]
#[derive(Clone, Debug)]
pub(crate) enum ProjKind {
    Deref,
    Field(FieldAccessKind),
    Index(SliceIndex<ValueRef>),
    Subslice { from: u64, to: u64, from_end: bool },
    Downcast(DowncastKind),
}

#[derive(Clone, Debug, dm::From)]
pub(crate) enum FieldAccessKind {
    Index(FieldIndex),
    PtrMetadata,
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

mod guards {
    use super::*;

    macro_rules! define_guard {
        ($base_type:ty, $ref_type:ty, $guarded_type:ty, $name: ident, $pattern:pat, $value_name:ident) => {
            #[derive(Clone, Debug)]
            pub(crate) struct $name<V>(pub V);

            impl<V> $name<V>
            where
                V: AsRef<$base_type>,
            {
                pub fn new(value: V) -> Self {
                    #![allow(unused_variables)]
                    core::assert_matches::assert_matches!(
                        value.as_ref(),
                        $pattern,
                        concat!("Value should be ", stringify!($guarded_type), ".")
                    );
                    Self(value)
                }

                pub fn $value_name(&self) -> &$guarded_type {
                    match self.0.as_ref() {
                        $pattern => $value_name,
                        _ => unreachable!(),
                    }
                }
            }

            impl<'a> $name<&'a mut $ref_type> {
                #[allow(unused)]
                pub fn make_mut(self) -> &'a mut $guarded_type {
                    match <$ref_type>::make_mut(self.0) {
                        $pattern => $value_name,
                        _ => unreachable!(),
                    }
                }
            }

            impl From<$name<$ref_type>> for $ref_type {
                fn from(value: $name<$ref_type>) -> Self {
                    value.0
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
    pub(crate) use define_guard;

    macro_rules! define_value_guard {
        ($guarded_type:ty, $name: ident, $pattern:pat, $value_name:ident) => {
            define_guard!(Value, ValueRef, $guarded_type, $name, $pattern, $value_name);

            impl<V> core::ops::Deref for $name<V>
            where
                V: AsRef<Value>,
            {
                type Target = $guarded_type;

                fn deref(&self) -> &Self::Target {
                    self.$value_name()
                }
            }

            impl<V> AsRef<$guarded_type> for $name<V>
            where
                Self: core::ops::Deref<Target = $guarded_type>,
            {
                fn as_ref(&self) -> &$guarded_type {
                    self
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
}
use guards::define_guard;

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

        pub(crate) fn unwrap_func_id(&self) -> FuncId {
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
                Zst | Str(..) | ByteStr(..) | Some => UnevalValue::Some.into(),
                _ => Self::Const(match val {
                    Bool(value) => value.into(),
                    Char(value) => value.into(),
                    Int { bit_rep, ty } => ConstValue::Int {
                        bit_rep: Wrapping(bit_rep),
                        ty,
                    },
                    Float { bit_rep, ty } => ConstValue::Float { bit_rep, ty },
                    Func(value) => ConstValue::Func(value),
                    Zst | Str(..) | ByteStr(..) | Some => unreachable!(),
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

    impl_conc_to_value_ref!(
        ConstValue,
        AdtValue,
        ArrayValue,
        FatPtrValue,
        UnevalValue,
        RawConcreteValue,
        PorterValue
    );

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

    impl<'a> TryFrom<&'a Value> for ValueType {
        type Error = &'a Value;

        fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
            match value {
                Value::Concrete(con_val) => ValueType::try_from(con_val).or(Err(value)),
                Value::Symbolic(sym_val) => ValueType::try_from(sym_val).or(Err(value)),
            }
        }
    }

    impl<'a> TryFrom<&'a SymValue> for ValueType {
        type Error = &'a SymValue;

        fn try_from(value: &'a SymValue) -> Result<Self, Self::Error> {
            match value {
                SymValue::Variable(SymbolicVar { id: _, ty }) => Ok(ty.clone()),
                SymValue::Expression(expr) => match expr {
                    Expr::Unary {
                        operator: _,
                        operand,
                    } => ValueType::try_from(operand.as_ref()),
                    Expr::Binary(BinaryExpr { operator, operands }) => {
                        use abs::BinaryOp::*;
                        match operator {
                            Eq | Lt | Le | Ne | Ge | Gt => Ok(ValueType::Bool),
                            _ => ValueType::try_from(operands.first().as_ref()).map_err(|_| value),
                        }
                    }
                    Expr::Extension { ty, .. } | Expr::Extraction { ty, .. } => Ok(ty.clone()),
                    Expr::Ite {
                        condition: _,
                        if_target,
                        else_target,
                    } => Option::zip(
                        ValueType::try_from(if_target.as_ref()).ok(),
                        ValueType::try_from(else_target.as_ref()).ok(),
                    )
                    .map(|(if_ty, else_ty)| {
                        debug_assert_eq!(
                            if_ty, else_ty,
                            "The types of ITE operands must be the same."
                        );
                        if_ty
                    })
                    .ok_or(value),
                    _ => Err(value),
                },
            }
        }
    }

    impl<'a> TryFrom<&'a ConcreteValue> for ValueType {
        type Error = &'a ConcreteValue;

        fn try_from(value: &'a ConcreteValue) -> Result<Self, Self::Error> {
            match value {
                ConcreteValue::Const(const_value) => match const_value {
                    ConstValue::Bool(_) => Ok(ValueType::Bool),
                    ConstValue::Char(_) => Ok(ValueType::Char),
                    ConstValue::Int { bit_rep: _, ty } => Ok((*ty).into()),
                    ConstValue::Float { bit_rep: _, ty } => Ok((*ty).into()),
                    _ => Err(value),
                },
                ConcreteValue::Unevaluated(UnevalValue::Lazy(value)) if value.1.is_some() => {
                    Ok(value.1.clone().unwrap())
                }
                _ => Err(value),
            }
        }
    }

    impl TryFrom<abs::UnaryOp> for super::UnaryOp {
        type Error = abs::UnaryOp;

        fn try_from(value: abs::UnaryOp) -> Result<Self, Self::Error> {
            use abs::UnaryOp::*;
            match value {
                Neg => Ok(Self::Neg),
                Not => Ok(Self::Not),
                _ => Err(value),
            }
        }
    }

    impl From<super::UnaryOp> for abs::UnaryOp {
        fn from(value: super::UnaryOp) -> Self {
            use super::UnaryOp::*;
            match value {
                Neg => Self::Neg,
                Not => Self::Not,
            }
        }
    }

    impl From<MultiValueArray> for MultiValueTree {
        fn from(value: MultiValueArray) -> Self {
            Self::Array(value)
        }
    }

    impl From<MultiValueLeaf> for MultiValueTree {
        fn from(value: MultiValueLeaf) -> Self {
            SymbolicReadTree::Single(value)
        }
    }
}
