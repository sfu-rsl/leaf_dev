use std::collections::HashMap;

use super::{
    AssertKind, BinaryOp, BranchingMetadata, CastKind, Constraint, FieldIndex, FloatType, IntType,
    Local, Projection, UnaryOp, ValueType, VariantIndex,
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

    type TypeManager;
    type Place;
    type Operand;

    fn place(&mut self) -> Self::PlaceHandler<'_>;

    fn operand(&mut self) -> Self::OperandHandler<'_>;

    fn assign_to<'a>(
        &'a mut self,
        dest: <Self::AssignmentHandler<'a> as AssignmentHandler>::Place,
    ) -> Self::AssignmentHandler<'a>;

    fn branch(&mut self) -> Self::BranchingHandler<'_>;

    fn func_control(&mut self) -> Self::FunctionHandler<'_>;

    fn type_control(&mut self) -> &mut Self::TypeManager;
}

pub(crate) trait PlaceHandler {
    type Place;
    type ProjectionHandler<'a>;
    type MetadataHandler<'a>;

    fn of_local(self, local: Local) -> Self::Place;

    fn project_on<'a>(self, place: &'a mut Self::Place) -> Self::ProjectionHandler<'a>;

    fn metadata<'a>(self, place: &'a mut Self::Place) -> Self::MetadataHandler<'a>;
}

pub(crate) trait PlaceProjectionHandler: Sized {
    type Result = ();
    type Local;

    fn by(self, projection: Projection<Self::Local>) -> Self::Result;

    #[inline]
    fn deref(self) -> Self::Result {
        self.by(Projection::Deref)
    }

    #[inline]
    fn for_field(self, field: FieldIndex) -> Self::Result {
        self.by(Projection::Field(field))
    }

    #[inline]
    fn at_index(self, index: Self::Local) -> Self::Result {
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
    type ConstantHandler: ConstantHandler<Operand = Self::Operand>;
    type MetadataHandler<'a>;

    fn copy_of(self, place: Self::Place) -> Self::Operand;

    fn move_of(self, place: Self::Place) -> Self::Operand;

    fn const_from(self) -> Self::ConstantHandler;

    fn new_symbolic(self, ty: ValueType) -> Self::Operand;

    fn metadata<'a>(self, operand: &'a mut Self::Operand) -> Self::MetadataHandler<'a>;
}

pub(crate) trait ConstantHandler {
    type Operand;

    fn bool(self, value: bool) -> Self::Operand;

    fn char(self, value: char) -> Self::Operand;

    fn int(self, bit_rep: u128, ty: IntType) -> Self::Operand;

    fn float(self, bit_rep: u128, ty: FloatType) -> Self::Operand;

    fn str(self, value: &'static str) -> Self::Operand;

    fn byte_str(self, value: &'static [u8]) -> Self::Operand;

    fn func(self, id: u64) -> Self::Operand;

    fn zst(self) -> Self::Operand;
}

pub(crate) trait AssignmentHandler {
    type Place;
    type Operand;
    type Field = Self::Operand;

    fn use_of(self, operand: Self::Operand);

    fn repeat_of(self, operand: Self::Operand, count: usize);

    fn ref_to(self, place: Self::Place, is_mutable: bool);

    fn thread_local_ref_to(self);

    fn address_of(self, place: Self::Place, is_mutable: bool);

    fn len_of(self, place: Self::Place);

    fn cast_of(self, operand: Self::Operand, target: CastKind);

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

    fn tuple_from(self, fields: impl Iterator<Item = Self::Field>);

    fn adt_from(self, fields: impl Iterator<Item = Self::Field>, variant: Option<VariantIndex>);

    fn union_from(self, active_field: FieldIndex, value: Self::Field);

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
    type MetadataHandler;

    fn before_call(self, func: Self::Operand, args: impl Iterator<Item = Self::Operand>);

    fn enter(self, func: Self::Operand);
    /// Use this function only when you're constructing a function that just calls
    /// known, instrumented functions.
    fn internal_enter(self);

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

pub(crate) trait OutputGenerator<I, V> {
    fn generate(&mut self, answers: HashMap<I, V>);
}

pub(crate) trait TypeManager {
    type Key;
    type Value;

    fn get_type(&self, key: Self::Key) -> Self::Value;

    fn set_type(&mut self, key: Self::Key, value: Self::Value);
}

pub(crate) mod implementation {
    use super::super::*;
    use super::*;

    use std::marker::PhantomData;

    pub(crate) struct DefaultPlaceHandler<L = Local, P = Projection<Local>> {
        _phantom: PhantomData<(L, P)>,
    }

    impl<L, P> Default for DefaultPlaceHandler<L, P> {
        fn default() -> Self {
            Self {
                _phantom: Default::default(),
            }
        }
    }

    impl<L, P> PlaceHandler for DefaultPlaceHandler<L, P>
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

    impl<L, P> PlaceProjectionHandler for DefaultPlaceProjectionHandler<'_, Place<L, P>>
    where
        P: From<Projection<L>>,
    {
        type Local = L;

        fn by(self, projection: Projection<Self::Local>) {
            self.place.add_projection(projection.into())
        }
    }

    pub(crate) struct DefaultOperandHandler<'a, P, S> {
        create_symbolic: Box<dyn FnOnce(ValueType) -> S + 'a>,
        _phantom: PhantomData<P>,
    }

    impl<'a, P, S> DefaultOperandHandler<'a, P, S> {
        pub(crate) fn new(create_symbolic: Box<dyn FnOnce(ValueType) -> S + 'a>) -> Self {
            Self {
                create_symbolic,
                _phantom: Default::default(),
            }
        }
    }

    impl<Place, SymValue> OperandHandler for DefaultOperandHandler<'_, Place, SymValue> {
        type Operand = Operand<Place, Constant, SymValue>;
        type Place = Place;
        type ConstantHandler = DefaultConstantHandler<Self::Operand>;
        type MetadataHandler<'a> = ();

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
            Operand::Symbolic((self.create_symbolic)(ty))
        }

        fn metadata<'a>(self, operand: &'a mut Self::Operand) -> Self::MetadataHandler<'a> {}
    }

    pub(crate) struct DefaultConstantHandler<O>(PhantomData<O>);

    impl<O: From<Constant>> ConstantHandler for DefaultConstantHandler<O> {
        type Operand = O;

        fn bool(self, value: bool) -> Self::Operand {
            Constant::Bool(value).into()
        }

        fn char(self, value: char) -> Self::Operand {
            Constant::Char(value).into()
        }

        fn int(self, bit_rep: u128, ty: IntType) -> Self::Operand {
            Constant::Int { bit_rep, ty }.into()
        }

        fn float(self, bit_rep: u128, ty: FloatType) -> Self::Operand {
            Constant::Float { bit_rep, ty }.into()
        }

        fn str(self, value: &'static str) -> Self::Operand {
            Constant::Str(value).into()
        }

        fn byte_str(self, value: &'static [u8]) -> Self::Operand {
            Constant::ByteStr(value).into()
        }

        fn func(self, id: u64) -> Self::Operand {
            Constant::Func(id).into()
        }

        fn zst(self) -> Self::Operand {
            Constant::Zst.into()
        }
    }
}
