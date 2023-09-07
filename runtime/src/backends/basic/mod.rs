mod alias;
mod call;
pub(crate) mod expr;
pub(crate) mod logger;
pub(crate) mod operand;
mod place;
mod state;

use std::{cell::RefCell, ops::DerefMut, rc::Rc};

use crate::{
    abs::{
        self, backend::*, AssertKind, BasicBlockIndex, BranchingMetadata, CastKind, IntType, Local,
        UnaryOp, VariantIndex,
    },
    solvers::z3::Z3Solver,
    trace::ImmediateTraceManager,
    tyexp::{DefaultTypeManager, TypeInformation},
};

use self::{
    alias::{
        ValueRefBinaryExprBuilder as BinaryExprBuilder,
        ValueRefExprBuilder as OperationalExprBuilder,
    },
    expr::{
        builders::DefaultExprBuilder as ExprBuilder, prelude::*,
        proj::DefaultSymProjector as SymProjector,
    },
    operand::DefaultOperandHandler,
    state::{RawPointerVariableState, StackedLocalIndexVariablesState},
};

type TraceManager = Box<dyn abs::backend::TraceManager<BasicBlockIndex, ValueRef>>;

type BasicVariablesState =
    RawPointerVariableState<StackedLocalIndexVariablesState<SymProjector>, SymProjector>;

type BasicCallStackManager = call::BasicCallStackManager<BasicVariablesState>;

type TypeManager =
    Box<dyn abs::backend::TypeManager<Key = String, Value = Option<TypeInformation>>>;

#[cfg(place_addr)]
type Place = place::PlaceWithAddress;
#[cfg(not(place_addr))]
type Place = crate::abs::Place;
type Projection<L = Local> = crate::abs::Projection<L>;
#[cfg(place_addr)]
type PlaceHandler = place::BasicPlaceHandler;
#[cfg(not(place_addr))]
type PlaceHandler = crate::abs::backend::implementation::DefaultPlaceHandler;
type FullPlace = place::FullPlace<Place>;
type Operand = operand::Operand<Place>;

pub struct BasicBackend {
    call_stack_manager: BasicCallStackManager,
    trace_manager: TraceManager,
    current_constraints: Vec<Constraint>,
    expr_builder: Rc<RefCell<ExprBuilder>>,
    sym_id_counter: u32,
    type_manager: TypeManager,
}

impl BasicBackend {
    pub fn new() -> Self {
        let expr_builder = Rc::new(RefCell::new(expr::builders::new_expr_builder()));
        let sym_projector = Rc::new(RefCell::new(expr::proj::new_sym_projector()));
        Self {
            call_stack_manager: BasicCallStackManager::new(Box::new(move |id| {
                RawPointerVariableState::new(
                    StackedLocalIndexVariablesState::new(id, sym_projector.clone()),
                    sym_projector.clone(),
                )
            })),
            trace_manager: Box::new(
                ImmediateTraceManager::<BasicBlockIndex, u32, ValueRef>::new_basic(Box::new(
                    Z3Solver::new_in_global_context(),
                )),
            ),
            current_constraints: Vec::new(),
            expr_builder,
            sym_id_counter: 0,
            // FIXME: file IO operation is expensive and should not be done directly here
            type_manager: Box::new(DefaultTypeManager::new()),
        }
    }
}

impl RuntimeBackend for BasicBackend {
    type PlaceHandler<'a> = PlaceHandler
    where
        Self: 'a;

    type OperandHandler<'a> = DefaultOperandHandler<'a, Self::Place>
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
        Self::PlaceHandler::default()
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
            RefValue::Mut(FullPlace::new(place, self.vars_state.id()))
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

    fn cast_of(mut self, operand: Self::Operand, target: CastKind) {
        let value = self.get_operand_value(operand);
        let cast_value = self.expr_builder().cast(value.into(), target);
        self.set(cast_value.into())
    }

    fn binary_op_between(
        mut self,
        operator: crate::abs::BinaryOp,
        first: Self::Operand,
        second: Self::Operand,
        checked: bool,
    ) {
        let first_value = self.get_operand_value(first);
        let second_value = self.get_operand_value(second);
        let result_value =
            self.expr_builder()
                .binary_op((first_value, second_value).into(), operator, checked);
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
            /* FIXME: #87 */
            Value::Concrete(ConcreteValue::Adt(AdtValue {
                kind: AdtKind::Enum { variant },
                ..
            })) => self.set_value(ConstValue::from(*variant).into()),
            _ => unreachable!("Discriminant is only supposed to be called on (concrete) enums."),
        }
    }

    fn array_from(mut self, items: impl Iterator<Item = Self::Operand>) {
        let value = ConcreteValue::Array(ArrayValue {
            elements: items.map(|e| self.get_operand_value(e)).collect(),
        });
        self.set_value(value.into())
    }

    fn tuple_from(mut self, fields: impl Iterator<Item = Self::Operand>) {
        self.set_adt_value(AdtKind::Tuple, fields)
    }

    fn adt_from(
        mut self,
        fields: impl Iterator<Item = Self::Operand>,
        variant: Option<VariantIndex>,
    ) {
        let kind = match variant {
            Some(variant) => AdtKind::Enum { variant },
            None => AdtKind::Struct,
        };
        self.set_adt_value(kind, fields)
    }

    fn union_from(self, active_field: abs::FieldIndex, value: Self::Operand) {
        todo!("Unions are not yet supported. {active_field} = {value:?}")
    }

    // TODO: Need to add support for the Deinit MIR instruction to have this working properly.
    // This solution works for now to avoid crashes when samples are run.
    fn variant_index(mut self, variant_index: VariantIndex) {
        let value = Value::Concrete(ConcreteValue::Adt(AdtValue {
            kind: AdtKind::Enum {
                variant: variant_index,
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

    fn set_adt_value(
        &mut self,
        kind: AdtKind,
        fields: impl Iterator<Item = <Self as AssignmentHandler>::Operand>,
    ) {
        let value = Value::Concrete(ConcreteValue::Adt(AdtValue {
            kind,
            fields: fields
                .map(|f| self.get_operand_value(f))
                .map(Some)
                .collect(),
        }));
        self.set_value(value)
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

    /// This function provides runtime support for all 5 assertion kinds in the leaf compiler.
    /// See: https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/mir/enum.AssertKind.html
    fn assert(self, cond: Self::Operand, expected: bool, _assert_kind: AssertKind<Self::Operand>) {
        // For now, we will call this function before the assert occurs and assume that assertions always succeed.
        // TODO: add a result: bool parameter to this function, and add support for it using a panic hook.
        let cond_val = get_operand_value(self.vars_state, cond);
        if cond_val.is_symbolic() {
            let mut constraint = Constraint::Bool(cond_val.clone());
            if !expected {
                constraint = constraint.not();
            }

            self.current_constraints.push(constraint);
            self.trace_manager.notify_step(
                0, /* TODO: The unique index of the block we have entered. */
                self.current_constraints.drain(..).collect(),
            );
        }
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
        let first = self.parent.discriminant.clone();
        let second = value
            .into_const(self.parent.metadata.discr_as_int)
            .to_value_ref();
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
    fn take(mut self, result: bool) {
        /* FIXME: Bad smell! The branching traits structure prevents
         * us from having a simpler and cleaner handler.
         */
        if !self.parent.discriminant.is_symbolic() {
            return;
        }

        let mut constraint = Constraint::Bool(self.parent.discriminant.clone());
        if !result {
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
    fn into_const(self, discr_as_int: IntType) -> ConstValue;
}

impl BranchCaseValue for char {
    fn into_const(self, _discr_as_int: IntType) -> ConstValue {
        ConstValue::Char(self)
    }
}

macro_rules! impl_int_branch_case_value {
    ($($type:ty),*) => {
        $(
            impl BranchCaseValue for $type {
                fn into_const(self, discr_as_int: IntType) -> ConstValue {
                    ConstValue::new_int(self, discr_as_int)
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

    fn before_call(self, func: Self::Operand, args: impl Iterator<Item = Self::Operand>) {
        // we don't know whether func will be internal or external
        let func_val = match try_const_operand_value(func) {
            Some(func) => func,
            None => unimplemented!("handle when func may be a non-const function pointer"),
        };
        self.call_stack_manager
            .prepare_for_call(func_val, args.collect());
    }

    fn enter(self, func: Self::Operand) {
        let func_val = match try_const_operand_value(func) {
            Some(func) => func,
            None => unimplemented!("handle when func may be a non-const function pointer"),
        };
        self.call_stack_manager
            .notify_enter(EntranceKind::ByFuncId(func_val));
    }

    fn internal_enter(self) {
        self.call_stack_manager
            .notify_enter(EntranceKind::ForcedInternal);
    }

    fn ret(self) {
        self.call_stack_manager.pop_stack_frame();
    }

    fn after_call(self, result_dest: Self::Place) {
        self.call_stack_manager.finalize_call(result_dest);
    }
}

type ValueRef = expr::ValueRef;

type Constraint = crate::abs::Constraint<ValueRef>;

fn get_operand_value(vars_state: &mut dyn VariablesState, operand: Operand) -> ValueRef {
    match operand {
        // copy and move are the same, but only for now. see: https://github.com/rust-lang/unsafe-code-guidelines/issues/188
        Operand::Place(place, operand::PlaceUsage::Copy)
        | Operand::Place(place, operand::PlaceUsage::Move) => vars_state.copy_place(&place),
        Operand::Const(constant) => Into::<ConcreteValue>::into(constant).to_value_ref(),
        Operand::Symbolic(sym) => sym.into(),
    }
}

/// This function should only be used when you know your operand will be const
fn try_const_operand_value(operand: Operand) -> Option<ValueRef> {
    match operand {
        Operand::Const(constant) => Some(Into::<ConcreteValue>::into(constant).to_value_ref()),
        _ => None,
    }
}

trait VariablesState<P = Place, V = ValueRef> {
    fn id(&self) -> usize;

    /// Returns a copy of the value stored at the given place. May not physically copy the value
    /// but the returned value should be independently usable from the original value.
    fn copy_place(&self, place: &P) -> V;

    /// Returns the value stored at the given place. The place should not contain a value after
    /// this operation.
    fn take_place(&mut self, place: &P) -> V {
        self.try_take_place(place).unwrap()
    }

    /// Tries to take the value of a place if available.
    fn try_take_place(&mut self, place: &P) -> Option<V>;

    /// Sets the value of a place. Overwrites the previous value if any, also defines a new local
    /// variable if it does not exist.
    fn set_place(&mut self, place: &P, value: V);
}

enum EntranceKind {
    ForcedInternal,
    ByFuncId(ValueRef),
}

trait CallStackManager {
    fn notify_enter(&mut self, kind: EntranceKind);

    fn prepare_for_call(&mut self, func: ValueRef, args: Vec<Operand>);
    fn finalize_call(&mut self, result_dest: Place);

    fn pop_stack_frame(&mut self);

    fn top(&mut self) -> &mut dyn VariablesState;
}
