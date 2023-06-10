use std::marker::PhantomData;

use crate::abs::{
    backend::{ConstantHandler, OperandHandler},
    ValueType,
};

use super::{expr::SymValueRef, place::Place};

#[derive(Debug)]
pub(crate) enum Operand<SymValue = SymValueRef> {
    Place(Place, PlaceUsage),
    Const(Constant),
    Symbolic(SymValue),
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

impl<S> From<Constant> for Operand<S> {
    fn from(constant: Constant) -> Self {
        Self::Const(constant)
    }
}

pub(crate) struct DefaultOperandHandler<'a, SymValue = SymValueRef> {
    symbolic_creator: Box<dyn FnOnce(ValueType) -> SymValue + 'a>,
}

pub(crate) struct DefaultConstantHandler<O>(PhantomData<O>);

impl<'a, SymValue> DefaultOperandHandler<'a, SymValue> {
    pub(crate) fn new(symbolic_creator: Box<dyn FnOnce(ValueType) -> SymValue + 'a>) -> Self {
        Self { symbolic_creator }
    }
}

impl<SymValue> OperandHandler for DefaultOperandHandler<'_, SymValue> {
    type Operand = Operand<SymValue>;
    type Place = Place;
    type ConstantHandler = DefaultConstantHandler<Operand<SymValue>>;

    fn copy_of(self, place: Self::Place) -> Self::Operand {
        Operand::Place(place, PlaceUsage::Copy)
    }

    fn move_of(self, place: Self::Place) -> Self::Operand {
        Operand::Place(place, PlaceUsage::Move)
    }

    fn const_from(self) -> Self::ConstantHandler {
        DefaultConstantHandler(PhantomData)
    }

    fn new_symbolic(self, ty: ValueType) -> Self::Operand {
        Operand::Symbolic((self.symbolic_creator)(ty))
    }
}

impl<O: From<Constant>> ConstantHandler for DefaultConstantHandler<O> {
    type Operand = O;

    fn bool(self, value: bool) -> Self::Operand {
        (Constant::Bool(value)).into()
    }

    fn char(self, value: char) -> Self::Operand {
        (Constant::Char(value)).into()
    }

    fn int(self, bit_rep: u128, size: u64, is_signed: bool) -> Self::Operand {
        (Constant::Int {
            bit_rep,
            size,
            is_signed,
        })
        .into()
    }

    fn float(self, bit_rep: u128, ebits: u64, sbits: u64) -> Self::Operand {
        (Constant::Float {
            bit_rep,
            ebits,
            sbits,
        })
        .into()
    }

    fn str(self, value: &'static str) -> Self::Operand {
        (Constant::Str(value)).into()
    }

    fn func(self, id: u64) -> Self::Operand {
        (Constant::Func(id)).into()
    }
}
