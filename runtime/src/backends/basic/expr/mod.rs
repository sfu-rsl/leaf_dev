use std::rc::Rc;

use crate::abs::{BinaryOp, UnaryOp};

type ExprRef = Rc<Expr>;

enum Expr {
    Constant(ConstExpr),
    NonConstant(NonConstExpr),
}

enum ConstExpr {
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

enum NonConstExpr {
    Symbolic {
        id: u64,
    },

    Copy(ExprRef),

    Unary {
        op: UnaryOp,
        expr: ExprRef,
    },
    Binary {
        op: BinaryOp,
        left: ExprRef,
        right: ExprRef,
    },

    Cast,

    AddrOf(ExprRef),
    Ref(ExprRef),
    Deref(ExprRef),

    Discriminant {
        expr: ExprRef,
    },

    Field {
        expr: ExprRef,
        field: u64,
    },

    ArrayInit(ArrayInitExpr),
    Index {
        expr: ExprRef,
        index: ExprRef,
    },
    Slice {
        expr: ExprRef,
        from: ExprRef,
        to: ExprRef,
        from_end: bool,
    },
    Len(ExprRef),
}

enum ArrayInitExpr {
    Repeat { element: ExprRef, count: ExprRef },
    Elements { elements: Vec<ExprRef> },
}
