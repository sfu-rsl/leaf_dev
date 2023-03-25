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
