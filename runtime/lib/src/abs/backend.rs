use std::collections::HashMap;

use super::{
    AssertKind, BinaryOp, BranchingMetadata, CastKind, Constraint, FieldIndex, Local, PlaceUsage,
    Projection, SymVariable, TypeId, UnaryOp, VariantIndex,
};

pub(crate) trait RuntimeBackend {
    type PlaceHandler<'a>: for<'b> PlaceHandler<PlaceInfo<'b> = Self::PlaceInfo, Place = Self::Place>
    where
        Self: 'a;

    type OperandHandler<'a>: OperandHandler
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

    type PlaceInfo;
    type Place;
    type Operand;

    fn place(&mut self, usage: PlaceUsage) -> Self::PlaceHandler<'_>;

    fn operand(&mut self) -> Self::OperandHandler<'_>;

    fn assign_to<'a>(
        &'a mut self,
        dest: <Self::AssignmentHandler<'a> as AssignmentHandler>::Place,
    ) -> Self::AssignmentHandler<'a>;

    fn branch(&mut self) -> Self::BranchingHandler<'_>;

    fn func_control(&mut self) -> Self::FunctionHandler<'_>;
}

pub(crate) trait PlaceHandler {
    type PlaceInfo<'a>;
    type Place;
    type DiscriminablePlace = Self::Place;
    type Operand;

    fn from_info<'a>(self, info: Self::PlaceInfo<'a>) -> Self::Place;

    /// # Remarks
    /// Used for discriminant of enums.
    fn tag_of<'a>(self, info: Self::PlaceInfo<'a>) -> Self::DiscriminablePlace;

    fn from_ptr(self, ptr: Self::Operand, ptr_type_id: TypeId) -> Self::Place;
}

pub(crate) trait PlaceBuilder {
    type Place;
    type ProjectionHandler<'a>;
    type MetadataHandler<'a>;

    fn of_local(self, local: Local) -> Self::Place;

    fn project_on<'a>(self, place: &'a mut Self::Place) -> Self::ProjectionHandler<'a>;

    fn metadata<'a>(self, place: &'a mut Self::Place) -> Self::MetadataHandler<'a>;
}

pub(crate) trait PlaceProjectionHandler: Sized {
    type Result = ();
    type Index;

    fn by(self, projection: Projection<Self::Index>) -> Self::Result;

    #[inline]
    fn deref(self) -> Self::Result {
        self.by(Projection::Deref)
    }

    #[inline]
    fn for_field(self, field: FieldIndex) -> Self::Result {
        self.by(Projection::Field(field))
    }

    #[inline]
    fn at_index(self, index: Self::Index) -> Self::Result {
        self.by(Projection::Index(index))
    }

    #[inline]
    fn at_constant_index(self, offset: u64, min_length: u64, from_end: bool) -> Self::Result {
        self.by(Projection::ConstantIndex {
            offset,
            min_length,
            from_end,
        })
    }

    #[inline]
    fn subslice(self, from: u64, to: u64, from_end: bool) -> Self::Result {
        self.by(Projection::Subslice { from, to, from_end })
    }

    #[inline]
    fn downcast(self, variant: VariantIndex) -> Self::Result {
        self.by(Projection::Downcast(variant))
    }

    #[inline]
    fn opaque_cast(self) -> Self::Result {
        self.by(Projection::OpaqueCast)
    }

    #[inline]
    fn subtype(self) -> Self::Result {
        self.by(Projection::Subtype)
    }
}

pub(crate) trait OperandHandler {
    type Operand;
    type Place;
    type Constant = super::Constant;

    fn copy_of(self, place: Self::Place) -> Self::Operand;

    fn move_of(self, place: Self::Place) -> Self::Operand;

    fn const_from(self, info: Self::Constant) -> Self::Operand;

    fn new_symbolic(self, var: SymVariable<Self::Operand>) -> Self::Operand;
}

pub(crate) trait AssignmentHandler: Sized {
    type Place;
    type DiscriminablePlace = Self::Place;
    type Operand;
    type Field = Self::Operand;

    fn use_of(self, operand: Self::Operand);

    fn repeat_of(self, operand: Self::Operand, count: usize);

    fn ref_to(self, place: Self::Place, is_mutable: bool);

    fn thread_local_ref_to(self);

    // FIXME: Rename
    fn address_of(self, place: Self::Place, is_mutable: bool);

    fn len_of(self, place: Self::Place);

    fn cast_of(self, operand: Self::Operand, target: CastKind);

    fn binary_op_between(self, operator: BinaryOp, first: Self::Operand, second: Self::Operand);

    fn unary_op_on(self, operator: UnaryOp, operand: Self::Operand);

    fn discriminant_from(self, place: Self::DiscriminablePlace);

    fn array_from(self, items: impl Iterator<Item = Self::Operand>);

    fn tuple_from(self, fields: impl Iterator<Item = Self::Field>) {
        self.adt_from(fields, None)
    }

    fn adt_from(self, fields: impl Iterator<Item = Self::Field>, variant: Option<VariantIndex>);

    fn union_from(self, active_field: FieldIndex, value: Self::Field);

    fn closure_from(self, upvars: impl Iterator<Item = Self::Field>) {
        self.adt_from(upvars, None)
    }

    fn coroutine_from(self, upvars: impl Iterator<Item = Self::Field>) {
        self.adt_from(upvars, None)
    }

    fn coroutine_closure_from(self, upvars: impl Iterator<Item = Self::Field>) {
        self.adt_from(upvars, None)
    }

    fn raw_ptr_from(self, data_ptr: Self::Operand, metadata: Self::Operand, is_mutable: bool);

    fn variant_index(self, variant_index: VariantIndex);

    fn shallow_init_box_from(self, value: Self::Operand);

    fn use_if_eq(self, current: Self::Operand, expected: Self::Operand, then: Self::Operand);

    fn use_and_check_eq(self, val: Self::Operand, expected: Self::Operand);
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
    type Arg = Self::Operand;
    type MetadataHandler;

    fn before_call(
        self,
        func: Self::Operand,
        args: impl Iterator<Item = Self::Arg>,
        are_args_tupled: bool,
    );

    fn enter(
        self,
        func: Self::Operand,
        arg_places: impl Iterator<Item = Self::Place>,
        ret_val_place: Self::Place,
        tupled_arg: Option<(Local, super::TypeId)>,
    );

    fn override_return_value(self, value: Self::Operand);

    fn ret(self);

    fn after_call(self, result_dest: Self::Place);

    fn metadata(self) -> Self::MetadataHandler;
}

/// Keeps track of all the compounding constraints in a single trace
pub(crate) trait TraceManager<S, V> {
    fn notify_step(&mut self, step: S, new_constraints: Vec<Constraint<V>>);
}

pub(crate) trait PathInterestChecker<S> {
    fn is_interesting(&self, path: &[S]) -> bool;
}

/// A trait for the SMT solver.
/// It takes a set of constraints to check satisfiability of them together.
pub(crate) trait Solver<I, V> {
    fn check(&mut self, constraints: &[Constraint<V>]) -> SolveResult<I, V>;
}

/// The result of the checking performed by [`Solver`].
/// [`Sat`]: The constraints are satisfiable and a set of values from type `V`
/// for the variables identified by ids from type `I` is found.
/// [`Unsat`]: The constraints are unsatisfiable.
/// [`Unknown`]: The solver could not determine the satisfiability.
pub(crate) enum SolveResult<I, V> {
    Sat(HashMap<I, V>),
    Unsat,
    Unknown,
}

pub(crate) trait TypeManager {
    type Key;
    type Value;

    fn get_type(&self, key: Self::Key) -> Self::Value;
}

pub(crate) mod implementation {
    use super::super::*;
    use super::*;

    use std::marker::PhantomData;

    pub(crate) struct DefaultPlaceBuilder<L = Local, P = Projection<Local>> {
        _phantom: PhantomData<(L, P)>,
    }

    impl<L, P> Default for DefaultPlaceBuilder<L, P> {
        fn default() -> Self {
            Self {
                _phantom: Default::default(),
            }
        }
    }

    impl<L, P> PlaceBuilder for DefaultPlaceBuilder<L, P>
    where
        L: From<Local>,
        for<'a> L: 'a,
        for<'a> P: 'a,
    {
        type Place = Place<L, P>;
        type ProjectionHandler<'a> = DefaultPlaceProjectionHandler<'a, Self::Place>
        where Self::Place :'a;
        type MetadataHandler<'a> = ();

        fn of_local(self, local: Local) -> Self::Place {
            Place::new(local.into())
        }

        fn project_on<'a>(self, place: &'a mut Self::Place) -> Self::ProjectionHandler<'a> {
            DefaultPlaceProjectionHandler { place }
        }

        fn metadata(self, _place: &mut Self::Place) {}
    }

    pub(crate) struct DefaultPlaceProjectionHandler<'a, P> {
        place: &'a mut P,
    }

    impl<'a, L, P> DefaultPlaceProjectionHandler<'a, Place<L, P>> {
        pub(crate) fn new(place: &'a mut Place<L, P>) -> Self {
            Self { place }
        }
    }

    impl<L, I> PlaceProjectionHandler for DefaultPlaceProjectionHandler<'_, Place<L, Projection<I>>> {
        type Index = I;

        fn by(self, projection: Projection<Self::Index>) {
            self.place.add_projection(projection.into())
        }
    }
}
