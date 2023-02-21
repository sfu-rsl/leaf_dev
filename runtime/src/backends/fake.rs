use crate::abs::*;

pub(crate) struct FakeRuntime {}

impl Runtime for FakeRuntime {
    type PlaceHandler = FakePlaceHandler;
    type OperandHandler = FakeOperandHandler;
    type AssignmentHandler = FakeAssignmentHandler;

    fn place(&mut self) -> Self::PlaceHandler {
        unimplemented!()
    }

    fn operand(&mut self) -> Self::OperandHandler {
        unimplemented!()
    }

    fn assign_to(
        &mut self,
        dest: <Self::PlaceHandler as PlaceHandler>::Place,
    ) -> Self::AssignmentHandler {
        unimplemented!()
    }

    fn branch<T: Branching>() -> T {
        unimplemented!()
    }

    fn function<T: Function>() -> T {
        unimplemented!()
    }
}

pub(crate) struct FakePlaceHandler {}

impl PlaceHandler for FakePlaceHandler {
    type ProjectionHandler = FakePlaceProjectionHandler;

    type Place = <Self::ProjectionHandler as PlaceProjectionHandler>::Place;

    fn of_local(&mut self, local: Local) -> Self::Place {
        unimplemented!()
    }

    fn project_on(&mut self, place: Self::Place) -> Self::ProjectionHandler {
        unimplemented!()
    }
}

pub(crate) struct FakePlaceProjectionHandler {}

impl PlaceProjectionHandler for FakePlaceProjectionHandler {
    type Place = FakePlace;

    fn deref(&mut self) -> Self::Place {
        unimplemented!()
    }

    fn for_field(&mut self, field: u32) -> Self::Place {
        unimplemented!()
    }

    fn at_index(&mut self, index: Self::Place) -> Self::Place {
        unimplemented!()
    }

    fn at_constant_index(&mut self, offset: u64, min_length: u64, from_end: bool) -> Self::Place {
        unimplemented!()
    }

    fn subslice(&mut self, from: u64, to: u64, from_end: bool) -> Self::Place {
        unimplemented!()
    }

    fn downcast(&mut self, variant_index: u32) -> Self::Place {
        unimplemented!()
    }

    fn opaque_cast(&mut self) -> Self::Place {
        unimplemented!()
    }
}

pub(crate) struct FakeOperandHandler {}

impl OperandHandler for FakeOperandHandler {
    type Operand = FakeOperand;

    type Place = FakePlace;

    type ConstantHandler = FakeConstantHandler;

    fn copy_of(&mut self, place: Self::Place) -> Self::Operand {
        unimplemented!()
    }

    fn move_of(&mut self, place: Self::Place) -> Self::Operand {
        unimplemented!()
    }

    fn const_from(&mut self) -> Self::ConstantHandler {
        unimplemented!()
    }
}

pub(crate) struct FakeConstantHandler {}

impl ConstantHandler for FakeConstantHandler {
    type Operand = FakeOperand;

    fn bool(&mut self, value: bool) -> Self::Operand {
        unimplemented!()
    }

    fn char(&mut self, value: char) -> Self::Operand {
        unimplemented!()
    }

    fn int(&mut self, bit_rep: u128, size: u64, is_signed: bool) -> Self::Operand {
        unimplemented!()
    }

    fn float(&mut self, bit_rep: u128, ebits: u64, sbits: u64) -> Self::Operand {
        unimplemented!()
    }

    fn str(&mut self, value: &str) -> Self::Operand {
        unimplemented!()
    }

    fn func(&mut self, id: u64) -> Self::Operand {
        unimplemented!()
    }
}

pub(crate) struct FakeAssignmentHandler {}

impl AssignmentHandler for FakeAssignmentHandler {
    type Place = FakePlace;

    type Operand = FakeOperand;

    fn use_of(&mut self, operand: Self::Operand) {
        unimplemented!()
    }

    fn repeat_of(&mut self, operand: Self::Operand, count: usize) {
        unimplemented!()
    }

    fn ref_to(&mut self, place: Self::Place, is_mutable: bool) {
        unimplemented!()
    }

    fn thread_local_ref_to(&mut self) {
        unimplemented!()
    }

    fn address_of(&mut self, place: Self::Place, is_mutable: bool) {
        unimplemented!()
    }

    fn len_of(&mut self, place: Self::Place) {
        unimplemented!()
    }

    fn numeric_cast_of(&mut self, operand: Self::Operand, is_to_float: bool, size: usize) {
        unimplemented!()
    }

    fn cast_of(&mut self) {
        unimplemented!()
    }

    fn binary_op_between(
        &mut self,
        operator: BinaryOp,
        first: Self::Operand,
        second: Self::Operand,
        checked: bool,
    ) {
        unimplemented!()
    }

    fn unary_op_on(&mut self, operator: UnaryOp, operand: Self::Operand) {
        unimplemented!()
    }

    fn discriminant_of(&mut self, place: Self::Place) {
        unimplemented!()
    }

    fn array_from(&mut self, items: impl Iterator<Item = Self::Operand>) {
        unimplemented!()
    }
}

pub(crate) enum FakePlace {
    Local,
}

pub(crate) enum FakeOperand {
    Copy,
}
