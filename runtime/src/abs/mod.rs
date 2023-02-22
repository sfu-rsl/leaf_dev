pub(crate) type Local = u32;
pub type BasicBlockIndex = u32;
pub type VariantIndex = u32;

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    Offset,
}

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}

pub(crate) trait Runtime: Sized {
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

    fn function<T: Function>() -> T;
}

pub(crate) trait PlaceHandler {
    type Place;

    type ProjectionHandler<'a>: PlaceProjectionHandler<Place = Self::Place>
    where
        Self: 'a;

    fn of_local(&mut self, local: Local) -> Self::Place;

    fn project_on<'a>(&'a mut self, place: Self::Place) -> Self::ProjectionHandler<'a>;
}

pub(crate) trait PlaceProjectionHandler {
    type Place;

    fn deref(&mut self) -> Self::Place;

    fn for_field(&mut self, field: u32) -> Self::Place;

    fn at_index(&mut self, index: Self::Place) -> Self::Place;

    fn at_constant_index(&mut self, offset: u64, min_length: u64, from_end: bool) -> Self::Place;

    fn subslice(&mut self, from: u64, to: u64, from_end: bool) -> Self::Place;

    fn downcast(&mut self, variant_index: u32) -> Self::Place;

    fn opaque_cast(&mut self) -> Self::Place;
}

pub(crate) trait OperandHandler {
    type Operand;
    type Place;
    type ConstantHandler<'a>: ConstantHandler<Operand = Self::Operand>
    where
        Self: 'a;

    fn copy_of(&mut self, place: Self::Place) -> Self::Operand;

    fn move_of(&mut self, place: Self::Place) -> Self::Operand;

    fn const_from<'a>(&'a mut self) -> Self::ConstantHandler<'a>;
}

pub(crate) trait ConstantHandler {
    type Operand;

    fn bool(&mut self, value: bool) -> Self::Operand;

    fn char(&mut self, value: char) -> Self::Operand;

    fn int(&mut self, bit_rep: u128, size: u64, is_signed: bool) -> Self::Operand;

    fn float(&mut self, bit_rep: u128, ebits: u64, sbits: u64) -> Self::Operand;

    fn str(&mut self, value: &str) -> Self::Operand;

    fn func(&mut self, id: u64) -> Self::Operand;
}

pub(crate) trait AssignmentHandler {
    type Place;
    type Operand;

    fn use_of(&mut self, operand: Self::Operand);

    fn repeat_of(&mut self, operand: Self::Operand, count: usize);

    fn ref_to(&mut self, place: Self::Place, is_mutable: bool);

    fn thread_local_ref_to(&mut self);

    fn address_of(&mut self, place: Self::Place, is_mutable: bool);

    fn len_of(&mut self, place: Self::Place);

    fn numeric_cast_of(&mut self, operand: Self::Operand, is_to_float: bool, size: usize);

    fn cast_of(&mut self);

    fn binary_op_between(
        &mut self,
        operator: BinaryOp,
        first: Self::Operand,
        second: Self::Operand,
        checked: bool,
    );

    fn unary_op_on(&mut self, operator: UnaryOp, operand: Self::Operand);

    fn discriminant_of(&mut self, place: Self::Place);

    fn array_from(&mut self, items: impl Iterator<Item = Self::Operand>);
}

pub(crate) trait BranchingHandler {
    type BoolBranchTakingHandler<'a>: BranchTakingHandler<bool>
    where
        Self: 'a;
    type IntBranchTakingHandler<'a>: BranchTakingHandler<u128>
    where
        Self: 'a;
    type CharBranchTakingHandler<'a>: BranchTakingHandler<char>
    where
        Self: 'a;
    type EnumBranchTakingHandler<'a>: BranchTakingHandler<VariantIndex>
    where
        Self: 'a;

    fn on_bool<'a>(&'a mut self) -> Self::BoolBranchTakingHandler<'a>;

    fn on_int<'a>(&'a mut self) -> Self::IntBranchTakingHandler<'a>;

    fn on_char<'a>(&'a mut self) -> Self::CharBranchTakingHandler<'a>;

    fn on_enum<'a>(&'a mut self) -> Self::EnumBranchTakingHandler<'a>;
}

pub(crate) trait BranchTakingHandler<T> {
    fn take(&mut self, value: T);

    fn take_otherwise(&mut self, non_values: &[T]);
}

pub(crate) trait Function {}
