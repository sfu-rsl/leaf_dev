pub(crate) mod backend;
pub(crate) mod expr;
pub(crate) mod fmt;
pub(crate) mod place;

pub(crate) type LocalIndex = u32;
pub type BasicBlockIndex = u32;
pub type VariantIndex = u32;
pub type FieldIndex = u32;
pub type RawPointer = u64;
pub type PointerOffset = u64;
pub type TypeSize = PointerOffset;
pub type TypeId = core::any::TypeId;

pub(crate) type Local = place::Local;
pub(crate) type Place<L = Local, P = Projection<L>> = place::Place<L, P>;
pub(crate) type Projection<L> = place::Projection<L>;

#[derive(Debug)]
pub(crate) enum PlaceUsage {
    Copy,
    Move,
}

#[derive(Debug)]
pub(crate) enum Operand<P, C, S> {
    Place(P, PlaceUsage),
    Const(C),
    Symbolic(S),
}

#[derive(Debug)]
pub(crate) enum Constant {
    Bool(bool),
    Char(char),
    Int { bit_rep: u128, ty: IntType },
    Float { bit_rep: u128, ty: FloatType },
    Str(&'static str),
    ByteStr(&'static [u8]),
    Func(u64),
    Zst,
}

impl<P, C, S> From<Constant> for Operand<P, C, S>
where
    C: From<Constant>,
{
    fn from(value: Constant) -> Self {
        Self::Const(value.into())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    Offset,
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug)]
pub enum AssertKind<Operand> {
    BoundsCheck { len: Operand, index: Operand },
    Overflow(BinaryOp, Operand, Operand),
    OverflowNeg(Operand),
    DivisionByZero(Operand),
    RemainderByZero(Operand),
    ResumedAfterReturn(Operand), // NOTE: TODO: check if these exist in HIR only
    ResumedAfterPanic(Operand),  // NOTE: TODO: check if these exist in HIR only
}

pub(crate) struct BranchingMetadata {
    pub node_location: BasicBlockIndex,
    /* NOTE: If more type information was passed (such as reporting type for all local variables),
     * this field wouldn't be required. The main usage is for integer types, where
     * they are all compared to an u128. Also, if the backend is able to record
     * type information on the expressions, this field doesn't give any additional
     * information.
     */
    pub discr_as_int: IntType,
}

#[derive(Debug, Clone)]
pub(crate) enum Constraint<V> {
    Bool(V),
    Not(V),
}

impl<V> Constraint<V> {
    pub fn destruct_ref(&self) -> (&V, bool) {
        match self {
            Constraint::Bool(value) => (value, false),
            Constraint::Not(value) => (value, true),
        }
    }

    pub fn not(self) -> Constraint<V> {
        match self {
            Constraint::Bool(value) => Constraint::Not(value),
            Constraint::Not(value) => Constraint::Bool(value),
        }
    }
}

#[derive(Clone, Debug, derive_more::From)]
pub(crate) enum ValueType {
    Bool,
    Char,
    Int(IntType),
    Float(FloatType),
}

impl ValueType {
    pub(crate) fn new_int(bit_size: u64, is_signed: bool) -> Self {
        Self::Int(IntType {
            bit_size,
            is_signed,
        })
    }

    pub(crate) fn new_float(e_bits: u64, s_bits: u64) -> Self {
        Self::Float(FloatType { e_bits, s_bits })
    }
}

/* FIXME: These types will have a limited set of possible values. Thus they can be
 * optimized using techniques such as interning or even changing them to enums.
 * Enums are not much a favorable option, since they are against the abstract
 * representation of integers and floats in the engine.
 */
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) struct IntType {
    pub bit_size: u64,
    pub is_signed: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) struct FloatType {
    pub e_bits: u64,
    pub s_bits: u64,
}

pub(crate) static USIZE_TYPE: IntType = IntType {
    bit_size: std::mem::size_of::<usize>() as u64 * 8,
    is_signed: false,
};

#[derive(Clone, Debug)]
pub(crate) enum CastKind {
    ToChar,
    ToInt(IntType),
    ToFloat(FloatType),
    PointerUnsize,
}

impl TryFrom<CastKind> for ValueType {
    type Error = CastKind;

    fn try_from(value: CastKind) -> Result<Self, Self::Error> {
        match value {
            CastKind::ToChar => Ok(ValueType::Char),
            CastKind::ToInt(to) => Ok(ValueType::Int(to)),
            CastKind::ToFloat(to) => Ok(ValueType::Float(to)),
            _ => Err(value),
        }
    }
}
