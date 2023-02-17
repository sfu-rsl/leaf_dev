use crate::{BinaryOp, Local, UnaryOp};

pub trait Runtime: Sized {
    type PlaceHandler: PlaceHandler;
    type AssignmentHandler: AssignmentHandler;

    fn place(&mut self) -> Self::PlaceHandler;

    fn assign_to(dest: <Self::PlaceHandler as PlaceHandler>::Place) -> Self::AssignmentHandler;

    fn branch<T: Branching>() -> T;

    fn function<T: Function>() -> T;
}

pub trait PlaceHandler {
    type Place;

    type ProjectionHandler: PlaceProjectionHandler;

    fn of_local(&mut self, local: Local) -> Self::Place;

    fn project_on(&mut self, place: Self::Place) -> Self::ProjectionHandler;
}

pub trait PlaceProjectionHandler {
    type Place;

    fn deref(&mut self) -> Self::Place;

    fn for_field(&mut self, field: u32) -> Self::Place;

    fn at_index(&mut self, index: Self::Place) -> Self::Place;

    fn at_constant_index(&mut self, offset: u64, min_length: u64, from_end: bool) -> Self::Place;

    fn subslice(&mut self, from: u64, to: u64, from_end: bool) -> Self::Place;

    fn downcast(&mut self, variant_index: u32) -> Self::Place;

    fn opaque_cast(&mut self) -> Self::Place;
}

pub trait AssignmentHandler {
    type Place;
    type Operand;

    fn use_of(operand: Self::Operand);

    fn repeat_of(operand: Self::Operand, count: usize);

    fn ref_to(place: Self::Place, is_mutable: bool);

    fn thread_local_ref_to();

    fn address_of(place: Self::Place, is_mutable: bool);

    fn len_of(place: Self::Place);

    fn numeric_cast_of(operand: Self::Operand, is_to_float: bool, size: usize);

    fn cast_of();

    fn binary_op_between(
        operator: BinaryOp,
        first: Self::Operand,
        second: Self::Operand,
        checked: bool,
    );

    fn unary_op_on(operator: UnaryOp, operand: Self::Operand);

    fn discriminant_of(place: Self::Place);

    fn array_from(items: &[Self::Operand]);
}

pub trait Branching {}

pub trait Function {}

pub mod fake {
    use super::*;

    pub struct FakeRuntime {}

    impl Runtime for FakeRuntime {
        type PlaceHandler = FakePlaceHandler;

        type AssignmentHandler = FakeAssignmentHandler;

        fn place(&mut self) -> Self::PlaceHandler {
            unimplemented!()
        }

        fn assign_to(dest: <Self::PlaceHandler as PlaceHandler>::Place) -> Self::AssignmentHandler {
            unimplemented!()
        }

        fn branch<T: Branching>() -> T {
            unimplemented!()
        }

        fn function<T: Function>() -> T {
            unimplemented!()
        }
    }

    pub struct FakePlaceHandler {}

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

    pub struct FakePlaceProjectionHandler {}

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

        fn at_constant_index(
            &mut self,
            offset: u64,
            min_length: u64,
            from_end: bool,
        ) -> Self::Place {
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

    pub struct FakeAssignmentHandler {}

    impl AssignmentHandler for FakeAssignmentHandler {
        type Place = FakePlace;

        type Operand = FakeOperand;

        fn use_of(operand: Self::Operand) {
            unimplemented!()
        }

        fn repeat_of(operand: Self::Operand, count: usize) {
            unimplemented!()
        }

        fn ref_to(place: Self::Place, is_mutable: bool) {
            unimplemented!()
        }

        fn thread_local_ref_to() {
            unimplemented!()
        }

        fn address_of(place: Self::Place, is_mutable: bool) {
            unimplemented!()
        }

        fn len_of(place: Self::Place) {
            unimplemented!()
        }

        fn numeric_cast_of(operand: Self::Operand, is_to_float: bool, size: usize) {
            unimplemented!()
        }

        fn cast_of() {
            unimplemented!()
        }

        fn binary_op_between(
            operator: BinaryOp,
            first: Self::Operand,
            second: Self::Operand,
            checked: bool,
        ) {
            unimplemented!()
        }

        fn unary_op_on(operator: UnaryOp, operand: Self::Operand) {
            unimplemented!()
        }

        fn discriminant_of(place: Self::Place) {
            unimplemented!()
        }

        fn array_from(items: &[Self::Operand]) {
            unimplemented!()
        }
    }

    pub enum FakePlace {
    }

    pub enum FakeOperand {
    }
}
