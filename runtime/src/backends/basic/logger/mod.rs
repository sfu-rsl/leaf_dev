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

pub(crate) struct LoggerBackend {
    call_manager: CallManager,
}

impl LoggerBackend {
    pub fn new() -> Self {
        Self {
            call_manager: CallManager::new(),
        }
    }
}

impl RuntimeBackend for LoggerBackend {
    type PlaceHandler<'a> = DefaultPlaceHandler where Self: 'a;
    type OperandHandler<'a> = DefaultOperandHandler where Self : 'a;
    type AssignmentHandler<'a> = LoggerAssignmentHandler where Self : 'a;
    type BranchingHandler<'a> = LoggerBranchingHandler where Self : 'a;
    type FunctionHandler<'a> = LoggerFunctionHandler<'a> where Self: 'a;

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
        LoggerFunctionHandler {
            call_manager: &mut self.call_manager,
        }
    }
}

pub(crate) struct LoggerAssignmentHandler {
    destination: Place,
}

impl AssignmentHandler for LoggerAssignmentHandler {
    type Place = Place;
    type Operand = Operand;

    fn use_of(self, operand: Self::Operand) {
        self.log(operand);
    }

    fn repeat_of(self, operand: Self::Operand, count: usize) {
        self.log(format!("[{operand}] * {count}"));
    }

    fn ref_to(self, place: Self::Place, is_mutable: bool) {
        self.log(format!(
            "&{} {}",
            if is_mutable { "mut" } else { "" },
            place
        ))
    }

    fn thread_local_ref_to(self) {
        todo!()
    }

    fn address_of(self, place: Self::Place, is_mutable: bool) {
        self.log(format!(
            "addr {} {}",
            if is_mutable { "mut" } else { "" },
            place
        ))
    }

    fn len_of(self, place: Self::Place) {
        self.log(format!("len({place})"));
    }

    fn numeric_cast_of(self, operand: Self::Operand, is_to_float: bool, size: usize) {
        self.log(format!(
            "{} as {}{}",
            operand,
            if is_to_float { "f" } else { "i" },
            size
        ));
    }

    fn cast_of(self) {
        todo!()
    }

    fn binary_op_between(
        self,
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

    fn unary_op_on(self, operator: UnaryOp, operand: Self::Operand) {
        self.log(format!("{operator}{operand}"));
    }

    fn discriminant_of(self, place: Self::Place) {
        self.log(format!("{place}.discr"));
    }

    fn array_from(self, items: impl Iterator<Item = Self::Operand>) {
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
    type BoolBranchTakingHandler = LoggerBranchTakingHandler;

    type IntBranchTakingHandler = LoggerBranchTakingHandler;

    type CharBranchTakingHandler = LoggerBranchTakingHandler;

    type EnumBranchTakingHandler = LoggerBranchTakingHandler;

    fn on_bool(self) -> Self::BoolBranchTakingHandler {
        self.create_branch_taking()
    }

    fn on_int(self) -> Self::IntBranchTakingHandler {
        self.create_branch_taking()
    }

    fn on_char(self) -> Self::CharBranchTakingHandler {
        self.create_branch_taking()
    }

    fn on_enum(self) -> Self::EnumBranchTakingHandler {
        self.create_branch_taking()
    }
}

impl LoggerBranchingHandler {
    fn create_branch_taking(self) -> LoggerBranchTakingHandler {
        LoggerBranchTakingHandler {
            location: self.location,
            discriminant: self.discriminant,
        }
    }
}

pub(crate) struct LoggerBranchTakingHandler {
    location: BasicBlockIndex,
    discriminant: Operand,
}

impl BranchTakingHandler<bool> for LoggerBranchTakingHandler {
    fn take(self, value: bool) {
        self.log(format!(
            "{}{}",
            if value { "" } else { "!" },
            self.discriminant
        ));
    }

    fn take_otherwise(self, non_values: &[bool]) {
        self.take(!non_values[0])
    }
}

macro_rules! impl_general_branch_taking_handler {
    ($($type:ty),*) => {
        $(
            impl BranchTakingHandler<$type> for LoggerBranchTakingHandler {
                fn take(self, value: $type) {
                    self.log_eq(value);
                }

                fn take_otherwise(self, non_values: &[$type]) {
                    self.log_otherwise(non_values.iter())
                }
            }
        )*
    };
}

impl_general_branch_taking_handler!(u128, char, VariantIndex);

impl LoggerBranchTakingHandler {
    fn log_eq(&self, value: impl Display) {
        self.log(format!("{} == {}", self.discriminant, value));
    }

    fn log_otherwise(&self, non_values: impl Iterator<Item = impl Display>) {
        let non_values = non_values.collect::<Vec<_>>();
        if non_values.len() > 1 {
            self.log(format!(
                "{} not in {}",
                self.discriminant,
                comma_separated(non_values.iter())
            ));
        } else {
            self.log(format!("{} != {}", self.discriminant, non_values[0]));
        }
    }

    fn log(&self, message: impl Display) {
        log_info!("Took branch at {} because {}", self.location, message);
    }
}

pub(crate) struct LoggerFunctionHandler<'a> {
    call_manager: &'a mut CallManager,
}

impl FunctionHandler for LoggerFunctionHandler<'_> {
    type Place = Place;

    type Operand = Operand;

    fn call(
        self,
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
        self.call_manager
            .notify_call(CallInfo { func, result_dest });
    }

    fn ret(self) {
        let info = self.call_manager.notify_return();
        log_info!(
            "Returning from {} and storing result in {}",
            info.func,
            info.result_dest
        );
    }
}

struct CallInfo {
    func: Operand,
    result_dest: Place,
}

struct CallManager {
    stack: Vec<CallInfo>,
}

impl CallManager {
    fn new() -> Self {
        Self {
            /*
             * TODO: This is a hack to make sure that a call info exists for the
             * entry point. It will be investigated in #68.
            */
            stack: vec![CallInfo {
                func: Operand::Const(super::operand::Constant::Func(0)),
                result_dest: Place::Local(0),
            }],
        }
    }

    fn notify_call(&mut self, call: CallInfo) {
        self.stack.push(call);
    }

    fn notify_return(&mut self) -> CallInfo {
        self.stack.pop().unwrap()
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