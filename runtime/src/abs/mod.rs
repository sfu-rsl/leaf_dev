pub(crate) type Local = u32;

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
    type PlaceHandler: PlaceHandler;
    type OperandHandler: OperandHandler;
    type AssignmentHandler: AssignmentHandler<
        Place = <Self::PlaceHandler as PlaceHandler>::Place,
        Operand = <Self::OperandHandler as OperandHandler>::Operand,
    >;

    fn place(&mut self) -> Self::PlaceHandler;

    fn operand(&mut self) -> Self::OperandHandler;

    fn assign_to(
        &mut self,
        dest: <Self::PlaceHandler as PlaceHandler>::Place,
    ) -> Self::AssignmentHandler;

    fn branch<T: Branching>() -> T;

    fn function<T: Function>() -> T;
}

pub(crate) trait PlaceHandler {
    type Place;

    type ProjectionHandler: PlaceProjectionHandler;

    fn of_local(&mut self, local: Local) -> Self::Place;

    fn project_on(&mut self, place: Self::Place) -> Self::ProjectionHandler;
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
    type ConstantHandler;

    fn copy_of(&mut self, place: Self::Place) -> Self::Operand;

    fn move_of(&mut self, place: Self::Place) -> Self::Operand;

    fn const_from(&mut self) -> Self::ConstantHandler;
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

pub(crate) trait Branching {}

pub(crate) trait Function {}
