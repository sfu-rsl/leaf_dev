use crate::abs::{ConstantHandler, OperandHandler};

use super::place::Place;

#[derive(Debug)]
pub(crate) enum Operand {
    Place(Place, PlaceUsage),
    Const(Constant),
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

pub(crate) struct DefaultOperandHandler;

pub(crate) struct DefaultConstantHandler;

impl OperandHandler for DefaultOperandHandler {
    type Operand = Operand;
    type Place = Place;
    type ConstantHandler = DefaultConstantHandler;

    fn copy_of(self, place: Self::Place) -> Self::Operand {
        Operand::Place(place, PlaceUsage::Copy)
    }

    fn move_of(self, place: Self::Place) -> Self::Operand {
        Operand::Place(place, PlaceUsage::Move)
    }

    fn const_from(self) -> Self::ConstantHandler {
        DefaultConstantHandler
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
