use std::fmt::Display;

use crate::abs::{
    backend::*, AssertKind, BinaryOp, BranchingMetadata, Local, UnaryOp, VariantIndex,
};

use super::{
    operand::{DefaultOperandHandler, Operand, PlaceUsage},
    place::{DefaultPlaceHandler, LocalKind, Place, Projection},
};

use crate::utils::logging::log_info;

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
    type OperandHandler<'a> = DefaultOperandHandler where Self: 'a;
    type AssignmentHandler<'a> = LoggerAssignmentHandler where Self: 'a;
    type BranchingHandler<'a> = LoggerBranchingHandler where Self: 'a;
    type FunctionHandler<'a> = LoggerFunctionHandler<'a> where Self: 'a;

    type Place = Place;
    type Operand = Operand;

    fn place(&mut self) -> Self::PlaceHandler<'_> {
        DefaultPlaceHandler {}
    }

    fn operand(&mut self) -> Self::OperandHandler<'_> {
        DefaultOperandHandler
    }

    fn assign_to(&mut self, dest: Place) -> Self::AssignmentHandler<'_> {
        LoggerAssignmentHandler { destination: dest }
    }

    fn branch(&mut self) -> Self::BranchingHandler<'_> {
        LoggerBranchingHandler {}
    }

    fn func_control(&mut self) -> Self::FunctionHandler<'_> {
        LoggerFunctionHandler {
            call_manager: &mut self.call_manager,
        }
    }
}

// -----------------------------------

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

    fn char_cast_of(self, operand: Self::Operand) {
        self.log(format!("{operand} as char"));
    }

    fn integer_cast_of(self, operand: Self::Operand, is_signed: bool, bits: u64) {
        self.log(format!(
            "{} as {}{}",
            operand,
            if is_signed { "i" } else { "u" },
            bits,
        ));
    }

    fn float_cast_of(self, operand: Self::Operand, bits: u64) {
        self.log(format!("{operand} as f{bits}"));
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

    fn variant_index(self, variant_index: VariantIndex) {
        log_info!("{}.discr = index {}", self.destination, variant_index);
    }
}

impl LoggerAssignmentHandler {
    fn log(&self, message: impl Display) {
        log_info!("{} = {}", self.destination, message);
    }
}

// -----------------------------------

pub(crate) struct LoggerBranchingHandler {}

impl BranchingHandler for LoggerBranchingHandler {
    type Operand = Operand;
    type ConditionalBranchingHandler = LoggerConditionalBranchingHandler;

    fn conditional(
        self,
        discriminant: Self::Operand,
        metadata: BranchingMetadata,
    ) -> Self::ConditionalBranchingHandler {
        Self::ConditionalBranchingHandler {
            discriminant,
            metadata,
        }
    }

    fn assert(self, cond: Self::Operand, expected: bool, assert_kind: AssertKind<Self::Operand>) {
        log_info!(
            "Asserting {:?} based on {:?} == {}",
            assert_kind,
            cond,
            expected
        );
    }
}

pub(crate) struct LoggerConditionalBranchingHandler {
    discriminant: Operand,
    metadata: BranchingMetadata,
}

impl LoggerConditionalBranchingHandler {
    fn create_branch_taking(self) -> LoggerBranchTakingHandler {
        LoggerBranchTakingHandler {
            discriminant: self.discriminant,
            metadata: self.metadata,
        }
    }
}

impl ConditionalBranchingHandler for LoggerConditionalBranchingHandler {
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

pub(crate) struct LoggerBranchTakingHandler {
    discriminant: Operand,
    metadata: BranchingMetadata,
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
        log_info!(
            "Took branch at {} because {}",
            self.metadata.node_location,
            message
        );
    }
}

// -----------------------------------

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
        let args: Vec<Self::Operand> = args.collect();
        log_info!(
            "Calling {}({}) -> {}",
            func,
            comma_separated(args.iter()),
            result_dest
        );
        self.call_manager.notify_call(CallInfo {
            func,
            result_dest,
            num_args: args.len() as u32,
        });
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
    num_args: u32,
    result_dest: Place,
}

pub(crate) struct CallManager {
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
                num_args: 0,
                result_dest: Place::new(LocalKind::ReturnValue),
            }],
        }
    }

    pub fn to_local_kind(&self, local: Local) -> LocalKind {
        let stack_frame = self.stack.last().expect("Call stack is empty.");
        if local == 0_u32 {
            LocalKind::ReturnValue
        } else if local <= stack_frame.num_args {
            // the 1st argument should be numbered as the 0th
            LocalKind::Argument(local - 1)
        } else {
            // the 1st normal local (technically nth) should be numbered as 0th
            LocalKind::Normal(local - stack_frame.num_args - 1)
        }
    }

    fn notify_call(&mut self, call: CallInfo) {
        self.stack.push(call);
    }

    fn notify_return(&mut self) -> CallInfo {
        self.stack.pop().unwrap()
    }
}

// -----------------------------------

impl Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        PlaceFormatter::format(f, self)
    }
}

struct PlaceFormatter;
impl PlaceFormatter {
    fn format(f: &mut std::fmt::Formatter, place: &Place) -> std::fmt::Result {
        place
            .projections
            .iter()
            .try_for_each(|proj| Self::pre(proj, f))
            .and_then(|_| write!(f, "{}", place.local))
            .and_then(|_| {
                place
                    .projections
                    .iter()
                    .rev()
                    .try_for_each(|proj| Self::post(proj, f))
            })
    }

    fn pre(proj: &Projection, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match proj {
            Projection::Deref => f.write_str("*"),
            _ => Result::Ok(()),
        }
    }

    fn post(proj: &Projection, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match proj {
            Projection::Field(field) => write!(f, ".{field}"),
            Projection::Index(index) => write!(f, "[{index}]"),
            Projection::Subslice { from, to, from_end } => {
                write!(f, "[{}..{}{}]", from, to, if *from_end { "^" } else { "" })
            }
            Projection::ConstantIndex {
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
            Projection::Downcast(variant) => write!(f, " as {variant}th"),
            _ => Result::Ok(()),
        }
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Operand::Place(place, usage) => match usage {
                PlaceUsage::Copy => write!(f, "C({place})"),
                PlaceUsage::Move => write!(f, "{place}"),
            },
            Operand::Const(constant) => write!(f, "Const::{constant:?}"),
            Operand::Symbolic(symbolic) => write!(f, "Symbolic::{symbolic:?}"),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(match self {
            UnaryOp::Not => "!",
            UnaryOp::Neg => "-",
        })
    }
}

fn comma_separated<T: Display>(iter: impl Iterator<Item = T>) -> String {
    iter.map(|t| format!("{t}")).collect::<Vec<_>>().join(", ")
}
