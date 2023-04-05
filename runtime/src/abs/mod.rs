pub(crate) mod backend;

pub(crate) type Local = u32;
pub type BasicBlockIndex = u32;
pub type VariantIndex = u32;
pub type FieldIndex = u32;

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

pub(crate) struct BranchingMetadata {
    pub node_location: BasicBlockIndex,
    /* NOTE: If more type was passed (such as reporting type for all local variables),
     * this field wouldn't be required. The main usage is for integer types, where
     * they are all compared to an u128. Also, if the backend is able to record
     * type information on the expressions, this field doesn't give any additional
     * information.
     */
    pub discr_as_int: DiscriminantAsIntType,
}

pub struct DiscriminantAsIntType {
    pub bit_size: u64,
    pub is_signed: bool,
}

#[derive(Debug, Clone)]
pub(crate) enum Constraint<V> {
    Bool(V),
    Not(V),
}

impl<V> Constraint<V> {
    pub fn destruct(&self) -> (&V, bool) {
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

pub(crate) trait Translator<From, To> {
    fn translate(&mut self, value: &From) -> To;
}
