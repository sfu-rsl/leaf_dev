mod alias;
mod call;
mod config;
pub(crate) mod expr;
pub(crate) mod logger;
pub(crate) mod operand;
mod place;
mod state;

use std::{cell::RefCell, collections::HashMap, ops::DerefMut, rc::Rc};

use crate::{
    abs::{
        self, backend::*, place::HasMetadata, AssertKind, BasicBlockIndex, BranchingMetadata,
        CastKind, FieldIndex, IntType, Local, LocalIndex, PlaceUsage, RawPointer, TypeId, UnaryOp,
        VariantIndex,
    },
    solvers::z3::Z3Solver,
    trace::ImmediateTraceManager,
    tyexp::{self, TypeInfoExt},
};
use common::tyexp::{FieldsShapeInfo, StructShape, TypeExport, TypeInfo};

use self::{
    alias::{
        TypeManager, ValueRefBinaryExprBuilder as BinaryExprBuilder,
        ValueRefExprBuilder as OperationalExprBuilder,
    },
    config::BasicBackendConfig,
    expr::{
        builders::DefaultExprBuilder as ExprBuilder, prelude::*,
        proj::DefaultSymProjector as SymProjector,
    },
    operand::BasicOperandHandler,
    place::{BasicPlaceHandler, PlaceMetadata},
    state::{RawPointerVariableState, StackedLocalIndexVariablesState},
};

type TraceManager = Box<dyn abs::backend::TraceManager<BasicBlockIndex, ValueRef>>;

#[cfg(place_addr)]
type BasicVariablesState =
    RawPointerVariableState<StackedLocalIndexVariablesState<SymProjector>, SymProjector>;
#[cfg(not(place_addr))]
type BasicVariablesState = StackedLocalIndexVariablesState<SymProjector>;

type BasicCallStackManager = call::BasicCallStackManager<BasicVariablesState>;

#[cfg(place_addr)]
type Place = place::PlaceWithMetadata;
#[cfg(not(place_addr))]
type Place = crate::abs::Place;
type Projection<L> = crate::abs::Projection<L>;
#[cfg(place_addr)]
type PlaceHandler = place::BasicPlaceHandler;
#[cfg(not(place_addr))]
type PlaceHandler = crate::abs::backend::implementation::DefaultPlaceHandler;
type FullPlace = place::FullPlace<Place>;
type Operand<S = SymValueRef> = operand::Operand<Place, S>;
#[cfg(place_addr)]
type OperandHandler<'a, SymValue> = operand::BasicOperandHandler<'a, Place, SymValue>;
#[cfg(not(place_addr))]
type OperandHandler<'a, SymValue> =
    crate::abs::backend::implementation::DefaultOperandHandler<'a, Place, SymValue>;
pub(crate) type Field<S = SymValueRef> = Operand<S>;

pub struct BasicBackend {
    call_stack_manager: BasicCallStackManager,
    trace_manager: TraceManager,
    current_constraints: Vec<Constraint>,
    expr_builder: Rc<RefCell<ExprBuilder>>,
    sym_id_counter: u32,
    type_manager: Rc<dyn TypeManager>,
    config: BasicBackendConfig,
}

impl BasicBackend {
    pub fn new(config: BasicBackendConfig) -> Self {
        let expr_builder = Rc::new(RefCell::new(expr::builders::new_expr_builder()));
        let sym_projector = Rc::new(RefCell::new(expr::proj::new_sym_projector()));
        let type_manager_ref = Rc::new(BasicTypeManager::default());
        let type_manager = type_manager_ref.clone();
        Self {
            call_stack_manager: BasicCallStackManager::new(
                Box::new(move |id| {
                    #[cfg(place_addr)]
                    let vars_state = RawPointerVariableState::new(
                        StackedLocalIndexVariablesState::new(id, sym_projector.clone()),
                        sym_projector.clone(),
                        type_manager_ref.clone(),
                    );
                    #[cfg(not(place_addr))]
                    let vars_state =
                        StackedLocalIndexVariablesState::new(id, sym_projector.clone());

                    vars_state
                }),
                &config.call,
            ),
            trace_manager: Box::new(
                ImmediateTraceManager::<BasicBlockIndex, u32, ValueRef>::new_basic(Box::new(
                    Z3Solver::new_in_global_context(),
                )),
            ),
            current_constraints: Vec::new(),
            expr_builder,
            sym_id_counter: 0,
            type_manager,
            config,
        }
    }
}

impl RuntimeBackend for BasicBackend {
    type PlaceHandler<'a> = PlaceHandler
    where
        Self: 'a;

    type OperandHandler<'a> = BasicOperandHandler<'a, Self::Place>
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
        BasicOperandHandler::new(Box::new(move |ty| self.new_symbolic_value(ty)))
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
        BasicFunctionHandler::new(&mut self.call_stack_manager, self.type_manager.as_ref())
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
    type Field = Field;

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

    fn address_of(mut self, place: Self::Place, _is_mutable: bool) {
        let value = self.vars_state.copy_place(&place);
        if let Value::Symbolic(SymValue::Expression(Expr::Projection(_))) = value.as_ref() {
            let address_of_value = self.expr_builder().address_of(value.into());
            self.set(address_of_value.into())
        } else {
            #[cfg(abs_concrete)]
            let value = get_operand_value(self.vars_state, Operand::Const(abs::Constant::Some));
            #[cfg(not(abs_concrete))]
            let value = {
                #[cfg(place_addr)]
                let value = ConstValue::from(place.address().expect("Address is not available!"))
                    .to_value_ref();
                #[cfg(not(place_addr))]
                let value = unimplemented!("Addresses are not supported!");
                value
            };
            self.set(value);
        }
    }

    fn len_of(mut self, place: Self::Place) {
        let value = self.vars_state.copy_place(&place);
        let len_value = self.expr_builder().len(value.into());
        self.set(len_value.into())
    }

    fn cast_of(mut self, operand: Self::Operand, target: CastKind) {
        let value = self.get_operand_value(operand);

        #[cfg(abs_concrete)]
        if !value.is_symbolic() {
            let value = self.get_operand_value(abs::Constant::Some.into());
            return self.set(value);
        }

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
        let value = self.vars_state.copy_place(&place);
        let discr_value = self.expr_builder().discriminant(value.into());
        self.set(discr_value.into())
    }

    fn array_from(mut self, items: impl Iterator<Item = Self::Operand>) {
        let value = ConcreteValue::Array(ArrayValue {
            elements: items.map(|e| self.get_operand_value(e)).collect(),
        });
        self.set_value(value.into())
    }

    fn adt_from(
        mut self,
        fields: impl Iterator<Item = Self::Field>,
        variant: Option<VariantIndex>,
    ) {
        let kind = match variant {
            Some(variant) => AdtKind::Enum { variant },
            None => AdtKind::Struct,
        };
        self.set_adt_value(kind, fields)
    }

    fn union_from(self, active_field: abs::FieldIndex, value: Self::Field) {
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
        fields: impl Iterator<Item = <Self as AssignmentHandler>::Field>,
    ) {
        let value = Value::Concrete(ConcreteValue::Adt(AdtValue {
            kind,
            fields: fields
                .map(|f| AdtField {
                    value: Some(self.get_operand_value(f.into())),
                })
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
    type_manager: &'a dyn TypeManager,
}

impl<'a> BasicFunctionHandler<'a> {
    fn new(
        call_stack_manager: &'a mut impl CallStackManager,
        type_manager: &'a dyn TypeManager,
    ) -> Self {
        Self {
            call_stack_manager,
            type_manager,
        }
    }
}

impl<'a> FunctionHandler for BasicFunctionHandler<'a> {
    type Place = Place;
    type Operand = Operand;
    type MetadataHandler = BasicFunctionMetadataHandler<'a>;

    fn before_call(
        self,
        func: Self::Operand,
        args: impl Iterator<Item = Self::Arg>,
        are_args_tupled: bool,
    ) {
        // we don't know whether func will be internal or external
        let func_val = match try_const_operand_value(func) {
            Some(func) => func,
            None => unimplemented!("handle when func may be a non-const function pointer"),
        };
        let args = args
            .map(|a| get_operand_value(self.call_stack_manager.top(), a))
            .collect();
        self.call_stack_manager
            .prepare_for_call(func_val, args, are_args_tupled);
    }

    fn enter(self, func: Self::Operand) {
        let func_val = match try_const_operand_value(func) {
            Some(func) => func,
            None => unimplemented!("handle when func may be a non-const function pointer"),
        };
        self.call_stack_manager.notify_enter(func_val);
    }

    fn override_return_value(self, value: Self::Operand) {
        let value = get_operand_value(self.call_stack_manager.top(), value);
        self.call_stack_manager.override_return_value(value)
    }

    fn ret(self) {
        self.call_stack_manager.pop_stack_frame();
    }

    fn after_call(self, result_dest: Self::Place) {
        self.call_stack_manager.finalize_call(result_dest);
    }

    fn metadata(self) -> Self::MetadataHandler {
        BasicFunctionMetadataHandler {
            call_stack_manager: self.call_stack_manager,
            type_manager: self.type_manager,
        }
    }
}

pub(crate) struct BasicFunctionMetadataHandler<'a> {
    call_stack_manager: &'a mut dyn CallStackManager,
    type_manager: &'a dyn TypeManager,
}

impl BasicFunctionMetadataHandler<'_> {
    #[cfg(place_addr)]
    pub(crate) fn preserve_metadata(&mut self, place: Place) {
        let local = place.local();
        let metadata = local.metadata();
        let local: &abs::Local = local.as_ref();
        debug_assert!(
            local.is_func_local() && !place.has_projection(),
            "This method is meant for function locals not arbitrary places."
        );
        self.call_stack_manager
            .set_local_metadata(local, metadata.clone())
    }

    pub(crate) fn try_untuple_argument(&mut self, arg_index: LocalIndex, tuple_type_id: TypeId) {
        self.call_stack_manager
            .try_untuple_argument(arg_index, &|| {
                Box::new(BasicUntupleHelper::new(self.type_manager, tuple_type_id))
            })
    }
}

struct BasicUntupleHelper<'a> {
    type_manager: &'a dyn TypeManager,
    type_id: TypeId,
    type_info: Option<&'static TypeInfo>,
    fields_info: Option<&'static StructShape>,
}

impl UntupleHelper for BasicUntupleHelper<'_> {
    #[cfg(place_addr)]
    fn make_tupled_arg_pseudo_place_meta(&mut self, addr: RawPointer) -> PlaceMetadata {
        let mut metadata = PlaceMetadata::default();
        metadata.set_address(addr);
        Self::set_metadata_from_type_info(&mut metadata, self.type_info());
        metadata
    }

    fn num_fields(&self, tupled_value: &ValueRef) -> FieldIndex {
        if let Some(ref type_info) = self.type_info {
            let Some(FieldsShapeInfo::Struct(s)) = type_info.variants.first().map(|t| &t.fields)
            else {
                panic!("Expected tuple type info, got: {:?}", type_info)
            };
            s.fields.len() as FieldIndex
        } else if let Value::Concrete(ConcreteValue::Adt(AdtValue {
            kind: AdtKind::Struct,
            fields,
        })) = tupled_value.as_ref()
        {
            fields.len() as FieldIndex
        } else {
            panic!("Could not find the number of fields for the tupled arg.")
        }
    }

    fn field_place(&mut self, mut base: Place, field: FieldIndex) -> Place {
        use abs::backend::PlaceHandler;

        #[cfg(place_addr)]
        let base_addr = base.address();

        BasicPlaceHandler::default()
            .project_on(&mut base)
            .for_field(field);

        #[cfg(place_addr)]
        if self.type_info.is_some() {
            // FIXME: Repeated checks
            let field_info = &self.fields_info().fields[field as usize];
            let field_type = self.get_type(field_info.ty);
            Self::set_metadata_from_type_info(base.metadata_mut(), &field_type);
            base.metadata_mut()
                .set_address(base_addr.unwrap() + field_info.offset);
        }
        base
    }
}

impl<'a> BasicUntupleHelper<'a> {
    fn new(type_manager: &'a dyn TypeManager, type_id: TypeId) -> Self {
        Self {
            type_manager,
            type_id,
            type_info: None,
            fields_info: None,
        }
    }

    fn get_type(&self, type_id: TypeId) -> &'static TypeInfo {
        self.type_manager
            .get_type(type_id)
            .expect("Could not find type info for the tupled argument.")
    }

    fn type_info(&mut self) -> &'static TypeInfo {
        if self.type_info.is_none() {
            self.type_info = Some(self.get_type(self.type_id));
        }
        self.type_info.unwrap()
    }

    fn fields_info(&mut self) -> &'static StructShape {
        let type_info = self.type_info();
        self.fields_info
            .get_or_insert_with(|| match type_info.expect_single_variant().fields {
                FieldsShapeInfo::Struct(ref shape) => shape,
                _ => panic!("Expected tuple type info, got: {:?}", type_info),
            })
    }

    #[cfg(place_addr)]
    fn set_metadata_from_type_info(metadata: &mut PlaceMetadata, type_info: &TypeInfo) {
        metadata.set_type_id(type_info.id);
        metadata.set_size(type_info.size);
    }
}

pub(crate) struct BasicTypeManager<'t> {
    type_map: &'t HashMap<TypeId, TypeInfo>,
}

impl<'t> BasicTypeManager<'t> {
    fn new(type_map: &'t HashMap<TypeId, TypeInfo>) -> Self {
        Self { type_map }
    }
}

impl Default for BasicTypeManager<'static> {
    fn default() -> Self {
        Self::new(tyexp::instance::PROGRAM_TYPES.get_or_init(|| TypeExport::read()))
    }
}

impl<'t> crate::abs::backend::TypeManager for BasicTypeManager<'t> {
    type Key = TypeId;
    type Value = Option<&'t TypeInfo>;

    fn get_type(&self, key: Self::Key) -> Self::Value {
        self.type_map.get(&key)
    }
}

type ValueRef = expr::ValueRef;

type Constraint = crate::abs::Constraint<ValueRef>;

fn get_operand_value(vars_state: &mut dyn VariablesState, operand: Operand) -> ValueRef {
    match operand {
        // copy and move are the same, but only for now. see: https://github.com/rust-lang/unsafe-code-guidelines/issues/188
        Operand::Place(place, PlaceUsage::Copy) => vars_state.copy_place(&place),
        #[cfg(place_addr)]
        Operand::Place(place, PlaceUsage::Move) => vars_state.take_place(&place),
        #[cfg(not(place_addr))]
        Operand::Place(place, PlaceUsage::Move) => vars_state.copy_place(&place),
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
    fn take_place(&mut self, place: &P) -> V
    where
        P: self::state::PlaceRef,
        P::Local: std::fmt::Display,
    {
        self.try_take_place(place)
            .ok_or(self::state::PlaceError::LocalNotFound(place.local()))
            .unwrap()
    }

    /// Tries to take the value of a place if available.
    fn try_take_place(&mut self, place: &P) -> Option<V>;

    /// Sets the value of a place. Overwrites the previous value if any, also defines a new local
    /// variable if it does not exist.
    fn set_place(&mut self, place: &P, value: V);
}

trait CallStackManager {
    /* NOTE: Why `are_args_tupled` are passed? Isn't `try_untuple_argument` enough?
     * First, arguments are tupled at the call site, which also calls this function.
     * Second, when untupling, we should make sure that the arguments were tupled.
     * If closures are converted to a function pointer, then the arguments are not tupled.
     */
    fn prepare_for_call(&mut self, func: ValueRef, args: Vec<ValueRef>, are_args_tupled: bool);

    #[cfg(place_addr)]
    fn set_local_metadata(&mut self, local: &Local, metadata: PlaceMetadata);

    fn try_untuple_argument<'a, 'b>(
        &'a mut self,
        arg_index: LocalIndex,
        untuple_helper: &dyn Fn() -> Box<dyn UntupleHelper + 'b>,
    );

    fn notify_enter(&mut self, current_func: ValueRef);

    fn pop_stack_frame(&mut self);

    fn finalize_call(&mut self, result_dest: Place);

    fn override_return_value(&mut self, value: ValueRef);

    fn top(&mut self) -> &mut dyn VariablesState;
}

trait UntupleHelper {
    #[cfg(place_addr)]
    fn make_tupled_arg_pseudo_place_meta(&mut self, addr: RawPointer) -> PlaceMetadata;

    fn num_fields(&self, tupled_value: &ValueRef) -> FieldIndex;

    /// Takes a place and returns a place with projection to the field.
    /// It should make a valid place with full information needed for the state.
    fn field_place(&mut self, base: Place, field: FieldIndex) -> Place;
}
