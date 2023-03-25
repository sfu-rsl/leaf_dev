use std::collections::HashMap;

use super::{BasicBlockIndex, BinaryOp, Constraint, FieldIndex, Local, UnaryOp, VariantIndex};

pub(crate) trait RuntimeBackend: Sized {
    type PlaceHandler<'a>: PlaceHandler<Place = Self::Place>
    where
        Self: 'a;
    type OperandHandler<'a>: OperandHandler<Place = Self::Place, Operand = Self::Operand>
    where
        Self: 'a;
    type AssignmentHandler<'a>: AssignmentHandler<Place = Self::Place, Operand = Self::Operand>
    where
        Self: 'a;
    type BranchingHandler<'a>: BranchingHandler
    where
        Self: 'a;
    type FunctionHandler<'a>: FunctionHandler<Place = Self::Place, Operand = Self::Operand>
    where
        Self: 'a;

    type Place;
    type Operand;

    fn place<'a>(&'a mut self) -> Self::PlaceHandler<'a>;

    fn operand<'a>(&'a mut self) -> Self::OperandHandler<'a>;

    fn assign_to<'a>(
        &'a mut self,
        dest: <Self::AssignmentHandler<'a> as AssignmentHandler>::Place,
    ) -> Self::AssignmentHandler<'a>;

    fn branch<'a>(
        &'a mut self,
        location: BasicBlockIndex,
        discriminant: <Self::OperandHandler<'static> as OperandHandler>::Operand,
    ) -> Self::BranchingHandler<'a>;

    fn func_control<'a>(&'a mut self) -> Self::FunctionHandler<'a>;
}

pub(crate) trait PlaceHandler {
    type Place;

    type ProjectionHandler: PlaceProjectionHandler<Place = Self::Place>;

    fn of_local(self, local: Local) -> Self::Place;

    fn project_on(self, place: Self::Place) -> Self::ProjectionHandler;
}

pub(crate) trait PlaceProjectionHandler {
    type Place;

    fn deref(self) -> Self::Place;

    fn for_field(self, field: FieldIndex) -> Self::Place;

    fn at_index(self, index: Self::Place) -> Self::Place;

    fn at_constant_index(self, offset: u64, min_length: u64, from_end: bool) -> Self::Place;

    fn subslice(self, from: u64, to: u64, from_end: bool) -> Self::Place;

    fn downcast(self, variant_index: u32) -> Self::Place;

    fn opaque_cast(self) -> Self::Place;
}

pub(crate) trait OperandHandler {
    type Operand;
    type Place;
    type ConstantHandler: ConstantHandler<Operand = Self::Operand>;
    type SymbolicHandler: SymbolicHandler<Operand = Self::Operand>;

    fn copy_of(self, place: Self::Place) -> Self::Operand;

    fn move_of(self, place: Self::Place) -> Self::Operand;

    fn const_from(self) -> Self::ConstantHandler;

    fn symbolic(self) -> Self::SymbolicHandler;
}

pub(crate) trait ConstantHandler {
    type Operand;

    fn bool(self, value: bool) -> Self::Operand;

    fn char(self, value: char) -> Self::Operand;

    fn int(self, bit_rep: u128, size: u64, is_signed: bool) -> Self::Operand;

    fn float(self, bit_rep: u128, ebits: u64, sbits: u64) -> Self::Operand;

    fn str(self, value: &'static str) -> Self::Operand;

    fn func(self, id: u64) -> Self::Operand;
}

pub(crate) trait SymbolicHandler {
    type Operand;

    fn bool(self) -> Self::Operand;

    fn char(self) -> Self::Operand;

    fn int(self, size: u64, is_signed: bool) -> Self::Operand;

    fn float(self, ebits: u64, sbits: u64) -> Self::Operand;
}

pub(crate) trait AssignmentHandler {
    type Place;
    type Operand;

    fn use_of(self, operand: Self::Operand);

    fn repeat_of(self, operand: Self::Operand, count: usize);

    fn ref_to(self, place: Self::Place, is_mutable: bool);

    fn thread_local_ref_to(self);

    fn address_of(self, place: Self::Place, is_mutable: bool);

    fn len_of(self, place: Self::Place);

    fn numeric_cast_of(self, operand: Self::Operand, is_to_float: bool, size: usize);

    fn cast_of(self);

    fn binary_op_between(
        self,
        operator: BinaryOp,
        first: Self::Operand,
        second: Self::Operand,
        checked: bool,
    );

    fn unary_op_on(self, operator: UnaryOp, operand: Self::Operand);

    fn discriminant_of(self, place: Self::Place);

    fn array_from(self, items: impl Iterator<Item = Self::Operand>);

    fn variant_index(self, variant_index: VariantIndex);
}

pub(crate) trait BranchingHandler {
    type BoolBranchTakingHandler: BranchTakingHandler<bool>;
    type IntBranchTakingHandler: BranchTakingHandler<u128>;
    type CharBranchTakingHandler: BranchTakingHandler<char>;
    type EnumBranchTakingHandler: BranchTakingHandler<VariantIndex>;

    fn on_bool(self) -> Self::BoolBranchTakingHandler;

    fn on_int(self) -> Self::IntBranchTakingHandler;

    fn on_char(self) -> Self::CharBranchTakingHandler;

    fn on_enum(self) -> Self::EnumBranchTakingHandler;
}

pub(crate) trait BranchTakingHandler<T> {
    fn take(self, value: T);

    fn take_otherwise(self, non_values: &[T]);
}

pub(crate) trait FunctionHandler {
    type Place;
    type Operand;

    fn call(
        self,
        func: Self::Operand,
        args: impl Iterator<Item = Self::Operand>,
        result_dest: Self::Place,
    );

    fn ret(self);
}

pub(crate) trait TraceManager {
    type Step;
    type Value;

    fn notify_step(
        &mut self,
        step: Self::Step,
        new_constraints: impl Iterator<Item = Constraint<Self::Value>>,
    );
}

pub(crate) trait PathInterestChecker {
    type Step;

    fn is_interesting(&self, path: &[Self::Step]) -> bool;
}

pub(crate) trait Solver {
    type SymVarId;
    type Value;

    fn check(
        &mut self,
        constraints: &[Constraint<Self::Value>],
    ) -> SolveResult<Self::SymVarId, Self::Value>;
}

pub(crate) enum SolveResult<I, V> {
    Sat(HashMap<I, V>),
    Unsat,
    Unknown,
}
