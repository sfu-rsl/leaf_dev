use std::fmt::Display;

use crate::abs::{
    AssignmentHandler, BasicBlockIndex, BinaryOp, BranchTakingHandler, BranchingHandler,
    FunctionHandler, RuntimeBackend, UnaryOp, VariantIndex,
};

use super::{
    operand::{DefaultOperandHandler, Operand, PlaceUsage},
    place::{DefaultPlaceHandler, Place, ProjectionKind},
};

macro_rules! log_info {
    // Currently, we haven't added the support for transitive dependencies. Thus,
    // the logger library doesn't work.
    // ($($arg:tt)+) => (log::info!($($arg)+))
    ($($arg:tt)+) => (println!($($arg)+))
}

pub(crate) struct LoggerBackend {}

impl RuntimeBackend for LoggerBackend {
    type PlaceHandler<'a> = DefaultPlaceHandler where Self: 'a;
    type OperandHandler<'a> = DefaultOperandHandler where Self : 'a;
    type AssignmentHandler<'a> = LoggerAssignmentHandler where Self : 'a;
    type BranchingHandler<'a> = LoggerBranchingHandler where Self : 'a;
    type FunctionHandler<'a> = LoggerFunctionHandler where Self: 'a;

    type Place = Place;
    type Operand = Operand;

    fn place(&mut self) -> Self::PlaceHandler<'_> {
        DefaultPlaceHandler
    }

    fn operand(&mut self) -> Self::OperandHandler<'_> {
        DefaultOperandHandler
    }

    fn assign_to(&mut self, dest: Place) -> Self::AssignmentHandler<'_> {
        LoggerAssignmentHandler { destination: dest }
    }

    fn branch(
        &mut self,
        location: BasicBlockIndex,
        discriminant: Operand,
    ) -> Self::BranchingHandler<'_> {
        LoggerBranchingHandler {
            location,
            discriminant,
        }
    }

    fn func_control<'a>(&'a mut self) -> Self::FunctionHandler<'a> {
        LoggerFunctionHandler
    }
}

impl LoggerBackend {
    pub fn new() -> Self {
        Self {}
    }
}

pub(crate) struct LoggerAssignmentHandler {
    destination: Place,
}

impl AssignmentHandler for LoggerAssignmentHandler {
    type Place = Place;
    type Operand = Operand;

    fn use_of(&mut self, operand: Self::Operand) {
        self.log(operand);
    }

    fn repeat_of(&mut self, operand: Self::Operand, count: usize) {
        self.log(format!("[{operand}] * {count}"));
    }

    fn ref_to(&mut self, place: Self::Place, is_mutable: bool) {
        self.log(format!(
            "&{} {}",
            if is_mutable { "mut" } else { "" },
            place
        ))
    }

    fn thread_local_ref_to(&mut self) {
        todo!()
    }

    fn address_of(&mut self, place: Self::Place, is_mutable: bool) {
        self.log(format!(
            "addr {} {}",
            if is_mutable { "mut" } else { "" },
            place
        ))
    }

    fn len_of(&mut self, place: Self::Place) {
        self.log(format!("len({place})"));
    }

    fn numeric_cast_of(&mut self, operand: Self::Operand, is_to_float: bool, size: usize) {
        self.log(format!(
            "{} as {}{}",
            operand,
            if is_to_float { "f" } else { "i" },
            size
        ));
    }

    fn cast_of(&mut self) {
        todo!()
    }

    fn binary_op_between(
        &mut self,
        operator: BinaryOp,
        first: Self::Operand,
        second: Self::Operand,
        checked: bool,
    ) {
        self.log(format!(
            "{} {} {} {}",
            first,
            operator,
            second,
            if checked { "| ✓" } else { "" }
        ));
    }

    fn unary_op_on(&mut self, operator: UnaryOp, operand: Self::Operand) {
        self.log(format!("{operator}{operand}"));
    }

    fn discriminant_of(&mut self, place: Self::Place) {
        self.log(format!("{place}.discr"));
    }

    fn array_from(&mut self, items: impl Iterator<Item = Self::Operand>) {
        self.log(format!("[{}]", comma_separated(items)));
    }
}

impl LoggerAssignmentHandler {
    fn log(&self, message: impl Display) {
        log_info!("{} = {}", self.destination, message);
    }
}

pub(crate) struct LoggerBranchingHandler {
    location: BasicBlockIndex,
    discriminant: Operand,
}

impl BranchingHandler for LoggerBranchingHandler {
    type BoolBranchTakingHandler<'a> = LoggerBranchTakingHandler<'a>
    where
        Self: 'a;

    type IntBranchTakingHandler<'a> = LoggerBranchTakingHandler<'a>
    where
        Self: 'a;

    type CharBranchTakingHandler<'a> = LoggerBranchTakingHandler<'a>
    where
        Self: 'a;

    type EnumBranchTakingHandler<'a> = LoggerBranchTakingHandler<'a>
    where
        Self: 'a;

    fn on_bool<'a>(&'a mut self) -> Self::BoolBranchTakingHandler<'a> {
        self.create_branch_taking()
    }

    fn on_int<'a>(&'a mut self) -> Self::IntBranchTakingHandler<'a> {
        self.create_branch_taking()
    }

    fn on_char<'a>(&'a mut self) -> Self::CharBranchTakingHandler<'a> {
        self.create_branch_taking()
    }

    fn on_enum<'a>(&'a mut self) -> Self::EnumBranchTakingHandler<'a> {
        self.create_branch_taking()
    }
}

impl LoggerBranchingHandler {
    fn create_branch_taking(&self) -> LoggerBranchTakingHandler {
        LoggerBranchTakingHandler {
            location: self.location,
            discriminant: &self.discriminant,
        }
    }
}

pub(crate) struct LoggerBranchTakingHandler<'a> {
    location: BasicBlockIndex,
    discriminant: &'a Operand,
}

impl BranchTakingHandler<bool> for LoggerBranchTakingHandler<'_> {
    fn take(&mut self, value: bool) {
        self.log(format!(
            "{}{}",
            if value { "" } else { "!" },
            self.discriminant
        ));
    }

    fn take_otherwise(&mut self, non_values: &[bool]) {
        self.take(!non_values[0])
    }
}

macro_rules! impl_general_branch_taking_handler {
    ($($type:ty),*) => {
        $(
            impl BranchTakingHandler<$type> for LoggerBranchTakingHandler<'_> {
                fn take(&mut self, value: $type) {
                    self.log_eq(value);
                }

                fn take_otherwise(&mut self, non_values: &[$type]) {
                    self.log_otherwise(non_values.iter())
                }
            }
        )*
    };
}

impl_general_branch_taking_handler!(u128, char, VariantIndex);

impl LoggerBranchTakingHandler<'_> {
    fn log_eq(&self, value: impl Display) {
        self.log(format!("{} == {}", self.discriminant, value));
    }

    fn log_otherwise(&self, non_values: impl Iterator<Item = impl Display>) {
        self.log(format!(
            "{} not in {}",
            self.discriminant,
            comma_separated(non_values)
        ));
    }

    fn log(&self, message: impl Display) {
        log_info!("Took branch at {} because {}", self.location, message);
    }
}

pub(crate) struct LoggerFunctionHandler;

impl FunctionHandler for LoggerFunctionHandler {
    type Place = Place;

    type Operand = Operand;

    fn call(
        &mut self,
        func: Self::Operand,
        args: impl Iterator<Item = Self::Operand>,
        result_dest: Self::Place,
    ) {
        log_info!(
            "Calling {}({}) -> {}",
            func,
            comma_separated(args),
            result_dest
        );
    }

    fn ret(&mut self) {
        log_info!("Returning");
    }
}

impl Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        PlaceFormatter::format(f, self)
    }
}

struct PlaceFormatter;
impl PlaceFormatter {
    fn format(f: &mut std::fmt::Formatter<'_>, place: &Place) -> std::fmt::Result {
        match place {
            Place::Local(local) => write!(f, "v{}", local),
            Place::Projection { kind, on } => Self::pre(kind, f)
                .and_then(|_| write!(f, "{}", on))
                .and_then(|_| Self::post(kind, f)),
        }
    }

    fn pre(pkind: &ProjectionKind, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match pkind {
            ProjectionKind::Deref => f.write_str("*"),
            _ => Result::Ok(()),
        }
    }

    fn post(pkind: &ProjectionKind, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match pkind {
            ProjectionKind::Field(field) => write!(f, ".{}", field),
            ProjectionKind::Index(index) => write!(f, "[{}]", index),
            ProjectionKind::Subslice { from, to, from_end } => {
                write!(f, "[{}..{}{}]", from, to, if *from_end { "^" } else { "" })
            }
            ProjectionKind::ConstantIndex {
                offset,
                min_length,
                from_end,
            } => {
                write!(
                    f,
                    "{{>{}}}[{}{}]",
                    min_length,
                    offset,
                    if *from_end { "^" } else { "" }
                )
            }
            ProjectionKind::Downcast(variant) => write!(f, " as {}th", variant),
            _ => Result::Ok(()),
        }
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Place(place, usage) => match usage {
                PlaceUsage::Copy => write!(f, "C({})", place),
                PlaceUsage::Move => write!(f, "{}", place),
            },
            Operand::Const(constant) => write!(f, "{:?}", constant),
            _ => Result::Ok(()),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Rem => "%",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::Offset => "->",
        })
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            UnaryOp::Not => "!",
            UnaryOp::Neg => "-",
        })
    }
}

fn comma_separated<T: Display>(iter: impl Iterator<Item = T>) -> String {
    iter.map(|t| format!("{}", t))
        .collect::<Vec<_>>()
        .join(", ")
}
