pub(crate) mod backend;
pub(crate) mod expr;
pub(crate) mod fmt;

pub(crate) type LocalIndex = u32;
pub type BasicBlockIndex = u32;
pub type VariantIndex = u32;
pub type FieldIndex = u32;
pub type RawPointer = u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Local {
    ReturnValue,          // 0
    Argument(LocalIndex), // 1-n
    Normal(LocalIndex),   // > n
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Place<L = Local, P = Projection<Local>> {
    local: L,
    projections: Vec<P>,
}

impl<L, P> Place<L, P> {
    pub fn new(local: L) -> Self {
        Self {
            local,
            /* As most of the places are just locals, we try not to allocate at start. */
            projections: Vec::with_capacity(0),
        }
    }

    #[inline]
    pub fn local(&self) -> &L {
        &self.local
    }

    #[inline]
    pub fn has_projection(&self) -> bool {
        !self.projections.is_empty()
    }

    #[inline]
    pub fn projections(&self) -> &[P] {
        &self.projections
    }

    #[inline]
    pub fn add_projection(&mut self, projection: P) {
        self.projections.push(projection);
    }

    #[inline]
    pub fn with_projection(mut self, projection: P) -> Self {
        self.add_projection(projection);
        self
    }
}

impl<L, P> From<L> for Place<L, P> {
    fn from(value: L) -> Self {
        Self::new(value)
    }
}

impl<P> TryFrom<Place<Local, P>> for Local {
    type Error = Place<Local, P>;

    fn try_from(value: Place<Local, P>) -> Result<Self, Self::Error> {
        if !value.has_projection() {
            Ok(value.local)
        } else {
            Err(value)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Projection<L = Local> {
    Field(FieldIndex),
    Deref,
    Index(L),
    ConstantIndex {
        offset: u64,
        min_length: u64,
        from_end: bool,
    },
    Subslice {
        from: u64,
        to: u64,
        from_end: bool,
    },
    Downcast(VariantIndex),
    OpaqueCast,
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

#[derive(Clone, Debug)]
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
