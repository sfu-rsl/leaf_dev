use std::collections::HashMap;

use super::{
    AssertKind, BinaryOp, BranchingMetadata, Constraint, FieldIndex, Local, UnaryOp, VariantIndex,
};

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
    type BranchingHandler<'a>: BranchingHandler<Operand = Self::Operand>
    where
        Self: 'a;
    type FunctionHandler<'a>: FunctionHandler<Place = Self::Place, Operand = Self::Operand>
    where
        Self: 'a;

    type Place;
    type Operand;

    fn place(&mut self) -> Self::PlaceHandler<'_>;

    fn operand(&mut self) -> Self::OperandHandler<'_>;

    fn assign_to<'a>(
        &'a mut self,
        dest: <Self::AssignmentHandler<'a> as AssignmentHandler>::Place,
    ) -> Self::AssignmentHandler<'a>;

    fn branch<'a>(&'a mut self) -> Self::BranchingHandler<'a>;

    fn func_control(&mut self) -> Self::FunctionHandler<'_>;
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

    fn char_cast_of(self, operand: Self::Operand);

    fn integer_cast_of(self, operand: Self::Operand, is_signed: bool, bits: u64);

    fn float_cast_of(self, operand: Self::Operand, bits: u64);

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

// https://en.wikipedia.org/wiki/Branch_(computer_science)
pub(crate) trait BranchingHandler {
    type Operand;
    type ConditionalBranchingHandler: ConditionalBranchingHandler;

    fn conditional(
        self,
        discriminant: Self::Operand,
        metadata: BranchingMetadata,
    ) -> Self::ConditionalBranchingHandler;

    fn assert(self, cond: Self::Operand, expected: bool, assert_kind: AssertKind<Self::Operand>);
}

pub(crate) trait ConditionalBranchingHandler {
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

    fn before_call(self, func: Self::Operand, args: impl Iterator<Item = Self::Operand>);

    fn enter(self);

    fn ret(self);

    fn after_call(self, result_dest: Self::Place);
}

pub(crate) trait TraceManager<S, V> {
    fn notify_step(&mut self, step: S, new_constraints: Vec<Constraint<V>>);
}

pub(crate) trait PathInterestChecker<S> {
    fn is_interesting(&self, path: &[S]) -> bool;
}

pub(crate) trait Solver<V, I> {
    fn check(&mut self, constraints: &[Constraint<V>]) -> SolveResult<I, V>;
}

pub(crate) enum SolveResult<I, V> {
    Sat(HashMap<I, V>),
    Unsat,
    Unknown,
}

pub(crate) trait OutputGenerator<I, V> {
    fn generate(&mut self, values: Vec<(&I, &V)>);
}
