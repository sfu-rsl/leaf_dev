use std::{collections::HashMap, fmt::Display};

use crate::{
    abs::{
        backend::*, AssertKind, BinaryOp, BranchingMetadata, CastKind, FuncId, TypeId, UnaryOp,
        ValueType, VariantIndex,
    },
    tyexp::TypeInfo,
};

use super::{Field, OperandHandler, Place, PlaceHandler};

use crate::utils::logging::log_info;

// Here we only store the type information for the symbolic value.
type Operand = super::operand::Operand<Place, ValueType>;

pub(crate) struct LoggerBackend {
    call_manager: CallManager,
    type_manager: LoggerTypeManager,
}

impl LoggerBackend {
    #[allow(unused)]
    pub fn new() -> Self {
        Self {
            call_manager: CallManager::new(),
            type_manager: LoggerTypeManager::new(),
        }
    }
}

impl RuntimeBackend for LoggerBackend {
    type PlaceHandler<'a> = PlaceHandler where Self: 'a;
    type OperandHandler<'a> = OperandHandler<'a, ValueType> where Self: 'a;
    type AssignmentHandler<'a> = LoggerAssignmentHandler where Self: 'a;
    type BranchingHandler<'a> = LoggerBranchingHandler where Self: 'a;
    type FunctionHandler<'a> = LoggerFunctionHandler<'a> where Self: 'a;
    type TypeHandler<'a> = &'a mut LoggerTypeManager;

    type Place = Place;
    type Operand = Operand;

    fn place(&mut self) -> Self::PlaceHandler<'_> {
        Self::PlaceHandler::default()
    }

    fn operand(&mut self) -> Self::OperandHandler<'_> {
        OperandHandler::new(Box::new(|ty| ty))
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

    fn type_control(&mut self) -> Self::TypeHandler<'_> {
        &mut self.type_manager
    }
}

// -----------------------------------

pub(crate) struct LoggerAssignmentHandler {
    destination: Place,
}

impl AssignmentHandler for LoggerAssignmentHandler {
    type Place = Place;
    type Operand = Operand;
    type Field = Field<ValueType>;

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

    fn cast_of(self, operand: Self::Operand, target: CastKind) {
        self.log(match target {
            CastKind::ToChar => format!("{operand} as char"),
            CastKind::ToInt(to) => format!("{operand} as {to:#?}"),
            CastKind::ToFloat(to) => format!("{operand} as {to:#?}"),
            CastKind::PointerUnsize => format!("{operand} as DST pointer"),
            CastKind::ExposeAddress => format!("{operand} as usizeᵖ"),
            CastKind::ToPointer(type_id) => format!("{operand} from {type_id:#?}"),
            CastKind::SizedDynamize => format!("{operand} as dyn*"),
            CastKind::Transmute(dst) => format!("{operand} as Ty({dst})"),
        });
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

    fn tuple_from(self, fields: impl Iterator<Item = Self::Field>) {
        self.log(format!("({})", comma_separated(fields)));
    }

    fn adt_from(self, fields: impl Iterator<Item = Self::Field>, variant: Option<VariantIndex>) {
        let fields = comma_separated(fields.enumerate().map(|(i, f)| format!("{i}: {f}")));
        match variant {
            Some(discr) => self.log(format!("{{ discr: {discr}, {fields}}}")),
            None => self.log(format!("{{{fields}}}")),
        }
    }

    fn union_from(self, active_field: crate::abs::FieldIndex, value: Self::Field) {
        self.log(format!("{{{active_field}: {value}}}"));
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
    type MetadataHandler = ();

    fn before_call(self, func: Self::Operand, args: impl Iterator<Item = Self::Operand>) {
        log_info!("Just before call {}({})", func, comma_separated(args));
        self.call_manager.notify_before_call(func);
    }

    fn enter(self, current_func: Self::Operand) {
        let _info = self.call_manager.notify_enter_call();
        log::info!("Entered function {}", current_func);
    }

    fn override_return_value(self, value: Self::Operand) {
        log::info!("Overriding return value to {}", value);
    }

    fn ret(self) {
        let info = self.call_manager.notify_return();
        log_info!("Returning from {}", info.func);
    }

    fn after_call(self, result_dest: Self::Place) {
        let info = self.call_manager.notify_after_call();
        log_info!(
            "Exited function {} and storing result in {}",
            info.func,
            result_dest
        );
    }

    fn metadata(self) -> Self::MetadataHandler {}
}

pub(crate) struct LoggerTypeManager {
    type_map: HashMap<TypeId, TypeInfo>,
}

impl LoggerTypeManager {
    fn new() -> Self {
        LoggerTypeManager {
            type_map: HashMap::new(),
        }
    }
}

impl TypeManager for LoggerTypeManager {
    type Key = TypeId;
    type Value = Option<TypeInfo>;

    fn get_type(&self, key: Self::Key) -> Self::Value {
        log::info!("Getting value for key: [{:#?}]", key);
        self.type_map.get(&key).cloned()
    }

    fn set_type(&mut self, key: Self::Key, value: Self::Value) {
        log::info!("Setting value: [{:#?}] for key: [{:#?}]", value, key);
        self.type_map.insert(key, value.expect("Invalid value"));
    }
}

struct CallInfo {
    func: Operand,
}

struct CallManager {
    stack: Vec<CallInfo>,
    last_called: Option<Operand>,
}

impl CallManager {
    fn new() -> Self {
        Self {
            stack: vec![],
            last_called: None,
        }
    }

    fn notify_before_call(&mut self, func: Operand) {
        self.last_called = Some(func);
    }

    fn notify_enter_call(&mut self) -> &CallInfo {
        let last_called = self
            .last_called
            .take()
            .unwrap_or(Operand::Const(crate::abs::Constant::Func(FuncId::MAX)));
        self.stack.push(CallInfo { func: last_called });
        self.stack.last().unwrap()
    }

    fn notify_return(&self) -> &CallInfo {
        self.stack.last().unwrap()
    }

    fn notify_after_call(&mut self) -> CallInfo {
        self.stack.pop().unwrap()
    }
}

// -----------------------------------

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use crate::abs::PlaceUsage;
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

pub(crate) fn comma_separated<T: Display>(iter: impl Iterator<Item = T>) -> String {
    iter.map(|t| format!("{t}")).collect::<Vec<_>>().join(", ")
}
