use crate::abs::{
    backend::{ConstantHandler, OperandHandler},
    ValueType,
};

use super::{expr::SymValueRef, place::Place};

#[derive(Debug)]
pub(crate) enum Operand {
    Place(Place, PlaceUsage),
    Const(Constant),
    /* NOTE: The symbolic value type can be replaced with a generic type. */
    Symbolic(SymValueRef),
}

#[derive(Debug)]
pub(crate) enum PlaceUsage {
    Copy,
    Move,
}

#[derive(Debug)]
pub(crate) enum Constant {
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

#[derive(Debug)]
pub(crate) enum Symbolic {
    Bool,
    Char,
    Int { size: u64, is_signed: bool },
    Float { ebits: u64, sbits: u64 },
}

pub(crate) struct DefaultOperandHandler;

pub(crate) struct DefaultConstantHandler;

impl OperandHandler for DefaultOperandHandler {
    type Operand = Operand;
    type Place = Place;
    type ConstantHandler = DefaultConstantHandler;
    type SymbolicHandler = DefaultSymbolicHandler;

    fn copy_of(self, place: Self::Place) -> Self::Operand {
        Operand::Place(place, PlaceUsage::Copy)
    }

    fn move_of(self, place: Self::Place) -> Self::Operand {
        Operand::Place(place, PlaceUsage::Move)
    }

    fn const_from(self) -> Self::ConstantHandler {
        DefaultConstantHandler
    }

    fn new_symbolic(self) -> Self::SymbolicHandler {
        DefaultSymbolicHandler
    }
}

impl ConstantHandler for DefaultConstantHandler {
    type Operand = Operand;

    fn bool(self, value: bool) -> Self::Operand {
        Self::create(Constant::Bool(value))
    }

    fn char(self, value: char) -> Self::Operand {
        Self::create(Constant::Char(value))
    }

    fn int(self, bit_rep: u128, size: u64, is_signed: bool) -> Self::Operand {
        Self::create(Constant::Int {
            bit_rep,
            size,
            is_signed,
        })
    }

    fn float(self, bit_rep: u128, ebits: u64, sbits: u64) -> Self::Operand {
        Self::create(Constant::Float {
            bit_rep,
            ebits,
            sbits,
        })
    }

    fn str(self, value: &'static str) -> Self::Operand {
        Self::create(Constant::Str(value))
    }

    fn func(self, id: u64) -> Self::Operand {
        Self::create(Constant::Func(id))
    }
}

impl DefaultConstantHandler {
    fn create(constant: Constant) -> Operand {
        Operand::Const(constant)
    }
}

pub(crate) struct DefaultSymbolicHandler;

impl SymbolicHandler for DefaultSymbolicHandler {
    type Operand = Operand;

    fn bool(self) -> Self::Operand {
        Self::create(Symbolic::Bool)
    }

    fn char(self) -> Self::Operand {
        Self::create(Symbolic::Char)
    }

    fn int(self, size: u64, is_signed: bool) -> Self::Operand {
        Self::create(Symbolic::Int { size, is_signed })
    }

    fn float(self, ebits: u64, sbits: u64) -> Self::Operand {
        Self::create(Symbolic::Float { ebits, sbits })
    }
}

impl DefaultSymbolicHandler {
    fn create(symbolic: Symbolic) -> Operand {
        Operand::Symbolic(symbolic)
    }
}
