mod alias;
mod call;
pub(crate) mod expr;
pub(crate) mod logger;
pub(crate) mod operand;
pub(crate) mod place;
mod state;

use std::{cell::RefCell, ops::DerefMut, rc::Rc};

use crate::{
    abs::{
        self, backend::*, AssertKind, BasicBlockIndex, BranchingMetadata, IntType, UnaryOp,
        VariantIndex,
    },
    solvers::z3::Z3Solver,
    trace::ImmediateTraceManager,
};

use self::{
    alias::{
        ValueRefBinaryExprBuilder as BinaryExprBuilder,
        ValueRefExprBuilder as OperationalExprBuilder,
    },
    call::BasicCallStackManager,
    expr::{
        builders::DefaultExprBuilder as ExprBuilder, proj::DefaultSymProjector as SymProjector,
        AdtKind, AdtValue, ArrayValue, ConcreteValue, ConstValue, RefValue, SymValue, SymValueRef,
        SymbolicVar, Value,
    },
    operand::{DefaultOperandHandler, Operand},
    place::{DefaultPlaceHandler, Place},
    state::MutableVariablesState,
};

type TraceManager = Box<dyn abs::backend::TraceManager<BasicBlockIndex, ValueRef>>;

pub struct BasicBackend {
    call_stack_manager: BasicCallStackManager<MutableVariablesState<SymProjector>>,
    trace_manager: TraceManager,
    current_constraints: Vec<Constraint>,
    expr_builder: Rc<RefCell<ExprBuilder>>,
    sym_id_counter: u32,
}

impl BasicBackend {
    pub fn new() -> Self {
        let expr_builder = Rc::new(RefCell::new(expr::builders::new_expr_builder()));
        let sym_projector = Rc::new(RefCell::new(expr::proj::new_sym_projector()));
        Self {
            call_stack_manager: BasicCallStackManager::<MutableVariablesState<SymProjector>>::new(
                Box::new(move || MutableVariablesState::new(sym_projector.clone())),
            ),
            trace_manager: Box::new(
                ImmediateTraceManager::<BasicBlockIndex, u32, ValueRef>::new_basic(Box::new(
                    Z3Solver::new_in_global_context(),
                )),
            ),
            current_constraints: Vec::new(),
            expr_builder,
            sym_id_counter: 0,
        }
    }
}

impl RuntimeBackend for BasicBackend {
    type PlaceHandler<'a> = DefaultPlaceHandler
    where
        Self: 'a;

    type OperandHandler<'a> = DefaultOperandHandler<'a>
    where
        Self: 'a;

    type AssignmentHandler<'a> = BasicAssignmentHandler<'a, ExprBuilder>
    where
        Self: 'a;

    type BranchingHandler<'a> = BasicBranchingHandler<'a, ExprBuilder>
    where
        Self: 'a;

    type FunctionHandler<'a> = BasicFunctionHandler<'a>
    where
        Self: 'a;

    type Place = Place;

    type Operand = Operand;

    fn place(&mut self) -> Self::PlaceHandler<'_> {
        DefaultPlaceHandler {}
    }

    fn operand(&mut self) -> Self::OperandHandler<'_> {
        DefaultOperandHandler::new(Box::new(move |ty| self.new_symbolic_value(ty)))
    }

    fn assign_to<'a>(
        &'a mut self,
        dest: <Self::AssignmentHandler<'a> as AssignmentHandler>::Place,
    ) -> Self::AssignmentHandler<'a> {
        BasicAssignmentHandler::new(
            dest,
            self.call_stack_manager.top(),
            self.expr_builder.clone(),
        )
    }

    fn branch(&mut self) -> Self::BranchingHandler<'_> {
        BasicBranchingHandler {
            vars_state: self.call_stack_manager.top(),
            trace_manager: &mut self.trace_manager,
            current_constraints: &mut self.current_constraints,
            expr_builder: self.expr_builder.clone(),
        }
    }

    fn func_control(&mut self) -> Self::FunctionHandler<'_> {
        BasicFunctionHandler::new(&mut self.call_stack_manager)
    }
}

impl BasicBackend {
    fn new_symbolic_value(&mut self, ty: abs::ValueType) -> SymValueRef {
        self.sym_id_counter += 1;
        SymValue::Variable(SymbolicVar::new(self.sym_id_counter, ty)).to_value_ref()
    }
}

pub(crate) struct BasicAssignmentHandler<'s, EB: OperationalExprBuilder> {
    dest: Place,
    vars_state: &'s mut dyn VariablesState,
    expr_builder: Rc<RefCell<EB>>,
}

impl<'s, EB: OperationalExprBuilder> BasicAssignmentHandler<'s, EB> {
    fn new(
        dest: Place,
        vars_state: &'s mut dyn VariablesState,
        expr_builder: Rc<RefCell<EB>>,
    ) -> Self {
        Self {
            dest,
            vars_state,
            expr_builder,
        }
    }
}

impl<EB: OperationalExprBuilder> AssignmentHandler for BasicAssignmentHandler<'_, EB> {
    type Place = Place;
    type Operand = Operand;

    fn use_of(mut self, operand: Self::Operand) {
        let value = self.get_operand_value(operand);
        self.set(value)
    }

    fn repeat_of(mut self, operand: Self::Operand, count: usize) {
        let element_value = self.get_operand_value(operand);
        /* NOTE: As we don't expect the count to be a large number, we currently,
         * don't optimize this by using a single element and a length.
         */
        let value = ConcreteValue::Array(ArrayValue {
            elements: vec![element_value; count],
        })
        .into();
        self.set_value(value)
    }

    fn ref_to(mut self, place: Self::Place, is_mutable: bool) {
        let value = ConcreteValue::Ref(if is_mutable {
            RefValue::Mut(place)
        } else {
            RefValue::Immut(self.vars_state.copy_place(&place))
        })
        .into();
        self.set_value(value)
    }

    fn thread_local_ref_to(self) {
        todo!()
    }

    fn address_of(self, _place: Self::Place, _is_mutable: bool) {
        todo!()
    }

    fn len_of(mut self, place: Self::Place) {
        let value = self.vars_state.copy_place(&place);
        let len_value = self.expr_builder().len(value.into());
        self.set(len_value.into())
    }

    fn char_cast_of(mut self, operand: Self::Operand) {
        let value = self.get_operand_value(operand);
        let cast_value = self.expr_builder().cast_to_char(value.into());
        self.set(cast_value.into())
    }

    fn integer_cast_of(mut self, operand: Self::Operand, is_signed: bool, bits: u64) {
        let value = self.get_operand_value(operand);
        let cast_value = self
            .expr_builder()
            .cast_to_int(value.into(), bits, is_signed);
        self.set(cast_value.into())
    }

    fn float_cast_of(mut self, operand: Self::Operand, bits: u64) {
        let value = self.get_operand_value(operand);
        let cast_value = self.expr_builder().cast_to_float(value.into(), bits);
        self.set(cast_value.into())
    }

    fn cast_of(self) {
        todo!()
    }

    fn binary_op_between(
        mut self,
        operator: crate::abs::BinaryOp,
        first: Self::Operand,
        second: Self::Operand,
        _checked: bool,
    ) {
        // TODO: Add support for checked operations.

        let first_value = self.get_operand_value(first);
        let second_value = self.get_operand_value(second);
        let result_value = self
            .expr_builder()
            .binary_op((first_value, second_value).into(), operator);
        self.set(result_value.into())
    }

    fn unary_op_on(mut self, operator: UnaryOp, operand: Self::Operand) {
        let value = self.get_operand_value(operand);
        let result_value = self.expr_builder().unary_op(value.into(), operator);
        self.set(result_value.into())
    }

    fn discriminant_of(mut self, place: Self::Place) {
        let host_value = self.vars_state.copy_place(&place);
        match host_value.as_ref() {
            Value::Concrete(ConcreteValue::Adt(AdtValue {
                kind: AdtKind::Enum {
                    discriminant: discr,
                },
                ..
            })) => self.set_value(ConstValue::from(*discr).into()),
            _ => unreachable!("Discriminant is only supposed to be called on (concrete) enums."),
        }
    }

    fn array_from(mut self, items: impl Iterator<Item = Self::Operand>) {
        let value = Value::Concrete(ConcreteValue::Array(ArrayValue {
            elements: items.map(|e| self.get_operand_value(e)).collect(),
        }));
        self.set_value(value)
    }

    // TODO: Need to add support for the Deinit MIR instruction to have this working properly.
    // This solution works for now to avoid crashes when samples are run.
    fn variant_index(mut self, variant_index: VariantIndex) {
        let value = Value::Concrete(ConcreteValue::Adt(AdtValue {
            kind: AdtKind::Enum {
                discriminant: variant_index,
            },
            fields: vec![],
        }));
        self.set_value(value)
    }
}

impl<EB: OperationalExprBuilder> BasicAssignmentHandler<'_, EB> {
    fn set(&mut self, value: ValueRef) {
        self.vars_state.set_place(&self.dest, value);
    }

    fn set_value(&mut self, value: Value) {
        self.set(ValueRef::new(value));
    }

    fn get_operand_value(&mut self, operand: <Self as AssignmentHandler>::Operand) -> ValueRef {
        get_operand_value(self.vars_state, operand)
    }

    fn expr_builder(&self) -> impl DerefMut<Target = EB> + '_ {
        self.expr_builder.as_ref().borrow_mut()
    }
}

pub(crate) struct BasicBranchingHandler<'a, EB: BinaryExprBuilder> {
    vars_state: &'a mut dyn VariablesState,
    trace_manager: &'a mut TraceManager,
    current_constraints: &'a mut Vec<Constraint>,
    expr_builder: Rc<RefCell<EB>>,
}

impl<'a, EB: BinaryExprBuilder> BranchingHandler for BasicBranchingHandler<'a, EB> {
    type Operand = Operand;
    type ConditionalBranchingHandler = BasicConditionalBranchingHandler<'a, EB>;

    fn conditional(
        self,
        discriminant: Operand,
        metadata: abs::BranchingMetadata,
    ) -> Self::ConditionalBranchingHandler {
        let disc = get_operand_value(self.vars_state, discriminant);
        BasicConditionalBranchingHandler::new(
            disc,
            metadata,
            self.trace_manager,
            self.current_constraints,
            self.expr_builder,
        )
    }

    fn assert(self, cond: Self::Operand, expected: bool, assert_kind: AssertKind<Self::Operand>) {
        todo!(
            "Implement logic for assertions. {:?} {} {:?}",
            cond,
            expected,
            assert_kind
        )
    }
}

pub(crate) struct BasicConditionalBranchingHandler<'a, EB: BinaryExprBuilder> {
    discriminant: ValueRef,
    metadata: BranchingMetadata,
    trace_manager: &'a mut TraceManager,
    current_constraints: &'a mut Vec<Constraint>,
    expr_builder: Rc<RefCell<EB>>,
}

impl<'a, EB: BinaryExprBuilder> BasicConditionalBranchingHandler<'a, EB> {
    fn new(
        discriminant: ValueRef,
        metadata: BranchingMetadata,
        trace_manager: &'a mut TraceManager,
        current_constraints: &'a mut Vec<Constraint>,
        expr_builder: Rc<RefCell<EB>>,
    ) -> Self {
        Self {
            discriminant,
            metadata,
            trace_manager,
            current_constraints,
            expr_builder,
        }
    }

    fn notify_constraint(&mut self, constraint: Constraint) {
        self.current_constraints.push(constraint);
        self.trace_manager.notify_step(
            0, /* TODO: The unique index of the block we have entered. */
            self.current_constraints.drain(..).collect(),
        );
    }
}

impl<'a, EB: BinaryExprBuilder> ConditionalBranchingHandler
    for BasicConditionalBranchingHandler<'a, EB>
{
    type BoolBranchTakingHandler = BasicBranchTakingHandler<'a, EB>;
    type IntBranchTakingHandler = BasicBranchTakingHandler<'a, EB>;
    type CharBranchTakingHandler = BasicBranchTakingHandler<'a, EB>;
    type EnumBranchTakingHandler = BasicBranchTakingHandler<'a, EB>;

    fn on_bool(self) -> Self::BoolBranchTakingHandler {
        BasicBranchTakingHandler { parent: self }
    }
    fn on_int(self) -> Self::IntBranchTakingHandler {
        BasicBranchTakingHandler { parent: self }
    }
    fn on_char(self) -> Self::CharBranchTakingHandler {
        BasicBranchTakingHandler { parent: self }
    }
    fn on_enum(self) -> Self::EnumBranchTakingHandler {
        BasicBranchTakingHandler { parent: self }
    }
}

pub(crate) struct BasicBranchTakingHandler<'a, EB: BinaryExprBuilder> {
    parent: BasicConditionalBranchingHandler<'a, EB>,
}

impl<EB: BinaryExprBuilder> BasicBranchTakingHandler<'_, EB> {
    fn create_equality_expr(&mut self, value: impl BranchCaseValue, eq: bool) -> ValueRef {
        let discr_as_int = &self.parent.metadata.discr_as_int;
        let first = self.parent.discriminant.clone();
        let second = value.into_const(discr_as_int).to_value_ref();
        if eq {
            self.expr_builder().eq((first, second).into())
        } else {
            self.expr_builder().ne((first, second).into())
        }
        .into()
    }

    fn expr_builder(&self) -> impl DerefMut<Target = EB> + '_ {
        self.parent.expr_builder.as_ref().borrow_mut()
    }
}

impl<EB: BinaryExprBuilder> BranchTakingHandler<bool> for BasicBranchTakingHandler<'_, EB> {
    fn take(mut self, value: bool) {
        /* FIXME: Bad smell! The branching traits structure prevents
         * us from having a simpler and cleaner handler.
         */
        if !self.parent.discriminant.is_symbolic() {
            return;
        }

        let mut constraint = Constraint::Bool(self.parent.discriminant.clone());
        if !value {
            constraint = constraint.not();
        }

        self.parent.notify_constraint(constraint);
    }

    fn take_otherwise(self, non_values: &[bool]) {
        // FIXME: Duplicate code
        self.take(!non_values[0])
    }
}

macro_rules! impl_general_branch_taking_handler {
    ($($type:ty),*) => {
        $(
            impl<EB: BinaryExprBuilder> BranchTakingHandler<$type>
                for BasicBranchTakingHandler<'_, EB>
            {
                fn take(mut self, value: $type) {
                    if !self.parent.discriminant.is_symbolic() {
                        return;
                    }

                    let expr = self.create_equality_expr(value as u128, true);
                    let constraint = Constraint::Bool(expr);
                    self.parent.notify_constraint(constraint);
                }

                fn take_otherwise(mut self, non_values: &[$type]) {
                    if !self.parent.discriminant.is_symbolic() {
                        return;
                    }

                    // Converting all non-equalities into a single constraint to keep the semantics.
                    let constraint = Constraint::Bool(
                        non_values.into_iter().fold(
                            ConstValue::Bool(true).to_value_ref(),
                            |acc, v| {
                                let expr = self.create_equality_expr(*v, false);
                                self.expr_builder().and((acc, expr).into()).into()
                            },
                        )
                    );
                    self.parent.notify_constraint(constraint);
                }
            }
        )*
    };
}

impl_general_branch_taking_handler!(u128, char, VariantIndex);

trait BranchCaseValue {
    fn into_const(self, discr_as_int: &IntType) -> ConstValue;
}

impl BranchCaseValue for char {
    fn into_const(self, _discr_as_int: &IntType) -> ConstValue {
        ConstValue::Char(self)
    }
}

macro_rules! impl_int_branch_case_value {
    ($($type:ty),*) => {
        $(
            impl BranchCaseValue for $type {
                fn into_const(self, discr_as_int: &IntType) -> ConstValue {
                    ConstValue::Int {
                        bit_rep: self as u128,
                        ty: *discr_as_int,
                    }
                }
            }
        )*
    };
}

impl_int_branch_case_value!(u128, VariantIndex);

pub(crate) struct BasicFunctionHandler<'a> {
    call_stack_manager: &'a mut dyn CallStackManager,
}

impl<'a> BasicFunctionHandler<'a> {
    fn new(call_stack_manager: &'a mut impl CallStackManager) -> Self {
        Self { call_stack_manager }
    }
}

impl FunctionHandler for BasicFunctionHandler<'_> {
    type Place = Place;
    type Operand = Operand;

    fn before_call(self, _func: Self::Operand, args: impl Iterator<Item = Self::Operand>) {
        self.call_stack_manager.update_args(args.collect());
    }

    fn enter(self) {
        self.call_stack_manager.push_stack_frame();
    }

    fn ret(self) {
        self.call_stack_manager.pop_stack_frame();
    }

    fn after_call(self, result_dest: Self::Place) {
        /* FIXME: May require a cleaner approach. */

        let (returned_val, is_external_function) = self.call_stack_manager.get_return_info();
        if is_external_function {
            todo!("handle the case when an external function is called")
        } else {
            if let Some(returned_val) = returned_val {
                self.call_stack_manager
                    .top()
                    .set_place(&result_dest, returned_val)
            } else {
                // this is the case where a function has no return value
            }
        }
    }
}

type ValueRef = expr::ValueRef;

type Constraint = crate::abs::Constraint<ValueRef>;

fn get_operand_value(vars_state: &mut dyn VariablesState, operand: Operand) -> ValueRef {
    match operand {
        // copy and move are the same, but only for now. see: https://github.com/rust-lang/unsafe-code-guidelines/issues/188
        Operand::Place(place, operand::PlaceUsage::Copy)
        | Operand::Place(place, operand::PlaceUsage::Move) => vars_state.copy_place(&place),
        Operand::Const(constant) => ValueRef::new(Value::Concrete(ConcreteValue::from_const(
            get_constant_value(&constant),
        ))),
        Operand::Symbolic(sym) => sym.into(),
    }
}

fn get_constant_value(constant: &operand::Constant) -> ConstValue {
    match constant {
        operand::Constant::Bool(value) => ConstValue::Bool(*value),
        operand::Constant::Char(value) => ConstValue::Char(*value),
        operand::Constant::Int { bit_rep, ty } => ConstValue::Int {
            bit_rep: *bit_rep,
            ty: *ty,
        },
        operand::Constant::Float { bit_rep, ty } => ConstValue::Float {
            bit_rep: *bit_rep,
            ty: *ty,
        },
        operand::Constant::Str(value) => ConstValue::Str(value),
        operand::Constant::Func(value) => ConstValue::Func(*value),
    }
}

trait VariablesState {
    /// Returns a copy of the value stored at the given place. May not physically copy the value
    /// but the returned value should be independently usable from the original value.
    fn copy_place(&self, place: &Place) -> ValueRef;

    /// Returns the value stored at the given place. The place should not contain a value after
    /// this operation.
    fn take_place(&mut self, place: &Place) -> ValueRef;

    /// Tries to take the value of a place if available.
    fn try_take_place(&mut self, place: &Place) -> Option<ValueRef>;

    /// Sets the value of a place. Overwrites the previous value if any, also defines a new local
    /// variable if it does not exist.
    fn set_place(&mut self, place: &Place, value: ValueRef);
}

trait CallStackManager {
    fn update_args(&mut self, args: Vec<Operand>);

    fn push_stack_frame(&mut self);

    fn pop_stack_frame(&mut self);

    fn get_return_info(&mut self) -> (Option<ValueRef>, bool);

    fn top(&mut self) -> &mut dyn VariablesState;
}
