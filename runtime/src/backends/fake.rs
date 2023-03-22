use crate::abs::*;

pub(crate) struct FakeBackend {}

impl FakeBackend {
    pub(crate) fn new() -> Self {
        Self {}
    }
}

impl RuntimeBackend for FakeBackend {
    type PlaceHandler<'a> = FakePlaceHandler where Self: 'a;
    type OperandHandler<'a> = FakeOperandHandler where Self : 'a;
    type AssignmentHandler<'a> = FakeAssignmentHandler where Self : 'a;
    type BranchingHandler<'a> = FakeBranchingHandler where Self : 'a;
    type FunctionHandler<'a> = FakeFunctionHandler where Self: 'a;

    type Place = FakePlace;
    type Operand = FakeOperand;

    fn place(&mut self) -> Self::PlaceHandler<'_> {
        unimplemented!()
    }

    fn operand(&mut self) -> Self::OperandHandler<'_> {
        unimplemented!()
    }

    fn assign_to(&mut self, dest: Self::Place) -> Self::AssignmentHandler<'_> {
        unimplemented!()
    }

    fn branch(
        &mut self,
        location: BasicBlockIndex,
        discriminant: Self::Operand,
    ) -> Self::BranchingHandler<'_> {
        unimplemented!()
    }

    fn func_control(&mut self) -> Self::FunctionHandler<'_> {
        unimplemented!()
    }
}

pub(crate) struct FakePlaceHandler {}

impl PlaceHandler for FakePlaceHandler {
    type Place = FakePlace;
    type ProjectionHandler = FakePlaceProjectionHandler;

    fn of_local(self, local: Local) -> Self::Place {
        unimplemented!()
    }

    fn project_on(self, place: Self::Place) -> Self::ProjectionHandler {
        unimplemented!()
    }
}

pub(crate) struct FakePlaceProjectionHandler {}

impl PlaceProjectionHandler for FakePlaceProjectionHandler {
    type Place = FakePlace;

    fn deref(self) -> Self::Place {
        unimplemented!()
    }

    fn for_field(self, field: u32) -> Self::Place {
        unimplemented!()
    }

    fn at_index(self, index: Self::Place) -> Self::Place {
        unimplemented!()
    }

    fn at_constant_index(self, offset: u64, min_length: u64, from_end: bool) -> Self::Place {
        unimplemented!()
    }

    fn subslice(self, from: u64, to: u64, from_end: bool) -> Self::Place {
        unimplemented!()
    }

    fn downcast(self, variant_index: u32) -> Self::Place {
        unimplemented!()
    }

    fn opaque_cast(self) -> Self::Place {
        unimplemented!()
    }
}

pub(crate) struct FakeOperandHandler {}

impl OperandHandler for FakeOperandHandler {
    type Operand = FakeOperand;

    type Place = FakePlace;

    type ConstantHandler = FakeConstantHandler;

    type SymbolicHandler = FakeSymbolicHandler;

    fn copy_of(self, place: Self::Place) -> Self::Operand {
        unimplemented!()
    }

    fn move_of(self, place: Self::Place) -> Self::Operand {
        unimplemented!()
    }

    fn const_from(self) -> Self::ConstantHandler {
        FakeConstantHandler
    }

    fn symbolic(self) -> Self::SymbolicHandler {
        FakeSymbolicHandler
    }
}

pub(crate) struct FakeConstantHandler;

impl ConstantHandler for FakeConstantHandler {
    type Operand = FakeOperand;

    fn bool(self, value: bool) -> Self::Operand {
        unimplemented!()
    }

    fn char(self, value: char) -> Self::Operand {
        unimplemented!()
    }

    fn int(self, bit_rep: u128, size: u64, is_signed: bool) -> Self::Operand {
        unimplemented!()
    }

    fn float(self, bit_rep: u128, ebits: u64, sbits: u64) -> Self::Operand {
        unimplemented!()
    }

    fn str(self, value: &str) -> Self::Operand {
        unimplemented!()
    }

    fn func(self, id: u64) -> Self::Operand {
        unimplemented!()
    }
}

pub(crate) struct FakeSymbolicHandler;

impl SymbolicHandler for FakeSymbolicHandler {
    type Operand = FakeOperand;

    fn bool(self) -> Self::Operand {
        unimplemented!()
    }

    fn char(self) -> Self::Operand {
        unimplemented!()
    }

    fn int(self, size: u64, is_signed: bool) -> Self::Operand {
        unimplemented!()
    }

    fn float(self, ebits: u64, sbits: u64) -> Self::Operand {
        unimplemented!()
    }
}

pub(crate) struct FakeAssignmentHandler {}

impl AssignmentHandler for FakeAssignmentHandler {
    type Place = FakePlace;

    type Operand = FakeOperand;

    fn use_of(self, operand: Self::Operand) {
        unimplemented!()
    }

    fn repeat_of(self, operand: Self::Operand, count: usize) {
        unimplemented!()
    }

    fn ref_to(self, place: Self::Place, is_mutable: bool) {
        unimplemented!()
    }

    fn thread_local_ref_to(self) {
        unimplemented!()
    }

    fn address_of(self, place: Self::Place, is_mutable: bool) {
        unimplemented!()
    }

    fn len_of(self, place: Self::Place) {
        unimplemented!()
    }

    fn numeric_cast_of(self, operand: Self::Operand, is_to_float: bool, size: usize) {
        unimplemented!()
    }

    fn cast_of(self) {
        unimplemented!()
    }

    fn binary_op_between(
        self,
        operator: BinaryOp,
        first: Self::Operand,
        second: Self::Operand,
        checked: bool,
    ) {
        unimplemented!()
    }

    fn unary_op_on(self, operator: UnaryOp, operand: Self::Operand) {
        unimplemented!()
    }

    fn discriminant_of(self, place: Self::Place) {
        unimplemented!()
    }

    fn array_from(self, items: impl Iterator<Item = Self::Operand>) {
        unimplemented!()
    }

    fn variant_index(self, variant_index: VariantIndex) {
        unimplemented!()
    }
}

pub(crate) struct FakeBranchingHandler {}

impl BranchingHandler for FakeBranchingHandler {
    type BoolBranchTakingHandler = FakeBranchTakingHandler;
    type IntBranchTakingHandler = FakeBranchTakingHandler;
    type CharBranchTakingHandler = FakeBranchTakingHandler;
    type EnumBranchTakingHandler = FakeBranchTakingHandler;

    fn on_bool(self) -> Self::BoolBranchTakingHandler {
        todo!()
    }

    fn on_int(self) -> Self::IntBranchTakingHandler {
        todo!()
    }

    fn on_char(self) -> Self::CharBranchTakingHandler {
        todo!()
    }

    fn on_enum(self) -> Self::EnumBranchTakingHandler {
        todo!()
    }
}

pub(crate) struct FakeBranchTakingHandler {}

impl BranchTakingHandler<bool> for FakeBranchTakingHandler {
    fn take(self, value: bool) {
        unimplemented!()
    }

    fn take_otherwise(self, non_values: &[bool]) {
        unimplemented!()
    }
}

impl BranchTakingHandler<u128> for FakeBranchTakingHandler {
    fn take(self, value: u128) {
        unimplemented!()
    }

    fn take_otherwise(self, non_values: &[u128]) {
        unimplemented!()
    }
}

impl BranchTakingHandler<char> for FakeBranchTakingHandler {
    fn take(self, value: char) {
        unimplemented!()
    }

    fn take_otherwise(self, non_values: &[char]) {
        unimplemented!()
    }
}

impl BranchTakingHandler<VariantIndex> for FakeBranchTakingHandler {
    fn take(self, value: VariantIndex) {
        unimplemented!()
    }

    fn take_otherwise(self, non_values: &[VariantIndex]) {
        unimplemented!()
    }
}

pub(crate) struct FakeFunctionHandler {}

impl FunctionHandler for FakeFunctionHandler {
    type Place = FakePlace;

    type Operand = FakeOperand;

    fn call(
        self,
        func: Self::Operand,
        args: impl Iterator<Item = Self::Operand>,
        result_dest: Self::Place,
    ) {
        unimplemented!()
    }

    fn ret(self) {
        unimplemented!()
    }
}

pub(crate) enum FakePlace {
    Local,
}

pub(crate) enum FakeOperand {
    Copy,
}
