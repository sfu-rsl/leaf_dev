mod alias;
mod call;
mod concrete;
mod config;
pub(crate) mod expr;
mod outgen;
mod place;
mod state;
mod types;

use std::{
    assert_matches::assert_matches,
    cell::{RefCell, RefMut},
    collections::HashMap,
    iter,
    ops::DerefMut,
    rc::Rc,
};

use crate::{
    abs::{
        self, backend::*, place::HasMetadata, AssertKind, BasicBlockIndex, BranchingMetadata,
        CastKind, FieldIndex, IntType, Local, LocalIndex, PlaceUsage, RawPointer, SymVariable,
        TypeId, UnaryOp, VariantIndex,
    },
    solvers::z3::Z3Solver,
    trace::ImmediateTraceManager,
    tyexp::{self, FieldsShapeInfoExt, TypeInfoExt},
    utils::alias::RRef,
};
use common::log_info;
use common::tyexp::{FieldsShapeInfo, StructShape, TypeExport, TypeInfo};

use self::{
    alias::{
        BasicExprBuilder, BasicSymExprBuilder, TypeManager,
        ValueRefBinaryExprBuilder as BinaryExprBuilder,
        ValueRefExprBuilder as OperationalExprBuilder,
    },
    concrete::BasicConcretizer,
    config::BasicBackendConfig,
    expr::{prelude::*, translators::z3::Z3ValueTranslator},
    place::PlaceMetadata,
    state::{make_sym_place_handler, RawPointerVariableState},
    types::BasicTypeManager,
};

type TraceManager = dyn abs::backend::TraceManager<BasicBlockIndex, ValueRef>;

type BasicVariablesState = RawPointerVariableState<BasicSymExprBuilder>;

type BasicCallStackManager = call::BasicCallStackManager<BasicVariablesState>;

type Place = place::PlaceWithMetadata;
type Projection = place::Projection;
pub(crate) use place::BasicPlaceBuilder;

pub struct BasicBackend {
    call_stack_manager: BasicCallStackManager,
    trace_manager: RRef<TraceManager>,
    expr_builder: RRef<BasicExprBuilder>,
    sym_values: RRef<HashMap<u32, (SymValueRef, ConcreteValueRef)>>,
    type_manager: Rc<dyn TypeManager>,
}

impl BasicBackend {
    pub fn new(config: BasicBackendConfig) -> Self {
        let type_manager_ref = Rc::new(BasicTypeManager::default());
        let expr_builder_ref = Rc::new(RefCell::new(expr::builders::new_expr_builder(
            type_manager_ref.clone(),
        )));
        let expr_builder = expr_builder_ref.clone();
        let sym_values_ref = Rc::new(RefCell::new(
            HashMap::<u32, (SymValueRef, ConcreteValueRef)>::new(),
        ));
        let all_sym_values = sym_values_ref.clone();
        let type_manager = type_manager_ref.clone();
        let mut output_generator = outgen::BasicOutputGenerator::new(&config.outputs);
        let trace_manager_ref = Rc::new(RefCell::new(ImmediateTraceManager::<
            BasicBlockIndex,
            u32,
            ValueRef,
            _,
        >::new(
            Box::new(crate::pathics::AllPathInterestChecker),
            Box::new(Z3Solver::new_in_global_context(|ctx| {
                Z3ValueTranslator::new(ctx)
            })),
            true,
            Box::new(move |mut answers| {
                // FIXME: Performance can be improved.
                let all_sym_values = all_sym_values.borrow();
                let missing_answers = all_sym_values
                    .iter()
                    .filter(|(id, _)| !answers.contains_key(id))
                    .map(|(id, (_, conc))| (*id, conc.clone().0))
                    .collect::<Vec<_>>();
                answers.extend(missing_answers);
                output_generator.generate(&answers)
            }),
        )));
        let trace_manager = trace_manager_ref.clone();

        let sym_place_handler_factory = |s| {
            Rc::new(RefCell::from(make_sym_place_handler(s, || {
                Box::new(BasicConcretizer::new(
                    expr_builder_ref.clone(),
                    trace_manager_ref.clone(),
                ))
            })))
        };
        let sym_read_handler_ref = sym_place_handler_factory(config.sym_place.read);
        let sym_write_handler_ref = sym_place_handler_factory(config.sym_place.write);
        Self {
            call_stack_manager: BasicCallStackManager::new(
                Box::new(move |_id| {
                    let vars_state = RawPointerVariableState::new(
                        type_manager_ref.clone(),
                        sym_read_handler_ref.clone(),
                        sym_write_handler_ref.clone(),
                        Rc::new(RefCell::new(expr::builders::to_sym_expr_builder(
                            expr_builder_ref.clone(),
                        ))),
                    );

                    vars_state
                }),
                &config.call,
            ),
            trace_manager,
            expr_builder,
            sym_values: sym_values_ref.clone(),
            type_manager,
        }
    }
}

impl RuntimeBackend for BasicBackend {
    type PlaceHandler<'a> = BasicPlaceHandler<'a>
    where
        Self: 'a;

    type OperandHandler<'a> = BasicOperandHandler<'a>
    where
        Self: 'a;

    type AssignmentHandler<'a> = BasicAssignmentHandler<'a, BasicExprBuilder>
    where
        Self: 'a;

    type BranchingHandler<'a> = BasicBranchingHandler<'a, BasicExprBuilder>
    where
        Self: 'a;

    type FunctionHandler<'a> = BasicFunctionHandler<'a>
    where
        Self: 'a;

    type PlaceInfo = Place;
    type Place = PlaceValueRef;
    type Operand = ValueRef;

    fn place(&mut self, usage: PlaceUsage) -> Self::PlaceHandler<'_> {
        BasicPlaceHandler {
            vars_state: self.call_stack_manager.top(),
            usage,
            type_manager: self.type_manager.as_ref(),
        }
    }

    fn operand(&mut self) -> Self::OperandHandler<'_> {
        BasicOperandHandler {
            vars_state: self.call_stack_manager.top(),
            sym_values: self.sym_values.clone(),
        }
    }

    fn assign_to<'a>(
        &'a mut self,
        dest: <Self::AssignmentHandler<'a> as AssignmentHandler>::Place,
    ) -> Self::AssignmentHandler<'a> {
        BasicAssignmentHandler::new(dest, self)
    }

    fn branch(&mut self) -> Self::BranchingHandler<'_> {
        BasicBranchingHandler::new(self)
    }

    fn func_control(&mut self) -> Self::FunctionHandler<'_> {
        BasicFunctionHandler::new(self)
    }
}

pub(crate) struct BasicPlaceHandler<'a> {
    vars_state: &'a mut dyn VariablesState,
    usage: PlaceUsage,
    type_manager: &'a dyn TypeManager,
}

impl PlaceHandler for BasicPlaceHandler<'_> {
    type PlaceInfo<'a> = Place;
    type Place = PlaceValueRef;
    type DiscriminablePlace = TagPlaceWithInfo;
    type Operand = ValueRef;

    fn from_info<'a>(self, info: Self::PlaceInfo<'a>) -> Self::Place {
        self.vars_state.ref_place(&info, self.usage)
    }

    fn tag_of<'a>(self, info: Self::PlaceInfo<'a>) -> Self::DiscriminablePlace {
        let mut place = info;
        let type_manager: &dyn TypeManager = self.type_manager;
        let ty = type_manager.get_type(place.metadata().unwrap_type_id());
        let tag_info = ty.tag.as_ref().unwrap();
        let metadata = {
            let mut meta = PlaceMetadata::default();
            meta.set_address(
                place
                    .address()
                    .wrapping_byte_add(tag_info.as_field.offset as usize),
            );
            let tag_ty = type_manager.get_type(tag_info.as_field.ty);
            meta.set_type_id(tag_ty.id);
            if let Ok(value_ty) = tag_ty.try_into() {
                meta.set_ty(value_ty);
            }
            meta.set_size(tag_ty.size);
            meta
        };
        place.add_projection(Projection::Field(0));
        place.push_metadata(metadata);
        TagPlaceWithInfo(self.from_info(place), tag_info)
    }

    fn from_ptr(self, ptr: Self::Operand, ptr_type_id: TypeId) -> Self::Place {
        self.vars_state
            .ref_place_by_ptr(ptr, ptr_type_id, self.usage)
    }
}

pub(crate) struct BasicOperandHandler<'a> {
    vars_state: &'a mut dyn VariablesState,
    sym_values: RRef<HashMap<u32, (SymValueRef, ConcreteValueRef)>>,
}

impl OperandHandler for BasicOperandHandler<'_> {
    type Place = PlaceValueRef;
    type Operand = ValueRef;

    fn copy_of(self, place: Self::Place) -> Self::Operand {
        self.vars_state.copy_place(&place)
    }

    fn move_of(self, place: Self::Place) -> Self::Operand {
        self.vars_state.take_place(&place)
    }

    fn const_from(self, info: Self::Constant) -> Self::Operand {
        ConcreteValue::from(info).to_value_ref()
    }

    fn new_symbolic(self, var: SymVariable<Self::Operand>) -> Self::Operand {
        let mut sym_values = self.sym_values.borrow_mut();
        let id = sym_values.len() as u32 + 1;
        let conc_val = var
            .conc_value
            .expect("Concrete value of symbolic variables is required.");
        assert_matches!(
            conc_val.as_ref(),
            Value::Concrete(ConcreteValue::Const(..)),
            "Only constant values are currently expected to be used as the concrete value."
        );
        let sym_val = SymValue::Variable(SymbolicVar::new(id, var.ty)).to_value_ref();
        let conc_val = ConcreteValueRef::new(conc_val);

        log_info!(
            "Introducing a new symbolic variable: {} = {}",
            sym_val,
            conc_val,
        );
        sym_values.insert(id, (sym_val.clone(), conc_val));
        sym_val.into()
    }
}

pub(crate) struct BasicAssignmentHandler<'s, EB: OperationalExprBuilder> {
    dest: PlaceValueRef,
    vars_state: &'s mut dyn VariablesState,
    expr_builder: RRef<EB>,
    type_manager: &'s dyn TypeManager,
}

impl<'s> BasicAssignmentHandler<'s, BasicExprBuilder> {
    fn new(dest: PlaceValueRef, backend: &'s mut BasicBackend) -> Self {
        Self {
            dest,
            vars_state: backend.call_stack_manager.top(),
            expr_builder: backend.expr_builder.clone(),
            type_manager: backend.type_manager.as_ref(),
        }
    }
}

impl<EB: OperationalExprBuilder> AssignmentHandler for BasicAssignmentHandler<'_, EB> {
    type Place = PlaceValueRef;
    type DiscriminablePlace = TagPlaceWithInfo;
    type Operand = ValueRef;

    fn use_of(mut self, operand: Self::Operand) {
        self.set(operand)
    }

    fn repeat_of(mut self, operand: Self::Operand, count: usize) {
        let value = if operand.is_symbolic() {
            ConcreteValue::Array(ArrayValue {
                elements: vec![operand; count],
            })
            .into()
        } else {
            UnevalValue::Some.into()
        };
        self.set_value(value)
    }

    fn ref_to(mut self, place: Self::Place, _is_mutable: bool) {
        let value = if place.is_symbolic() {
            Expr::Ref(SymPlaceValueRef::new(place)).into()
        } else {
            UnevalValue::Some.into()
        };
        self.set_value(value)
    }

    fn thread_local_ref_to(mut self) {
        // Thread local references cannot refer to symbolic places, so the reference is concrete.
        self.set_value(UnevalValue::Some.into())
    }

    fn address_of(self, place: Self::Place, is_mutable: bool) {
        // For symbolic values `ref_to` and `address_of` should have the same behavior.
        self.ref_to(place, is_mutable)
    }

    fn len_of(mut self, place: Self::Place) {
        let value = if place.is_symbolic() {
            Expr::Len(SymPlaceValueRef::new(place)).into()
        } else {
            UnevalValue::Some.into()
        };
        self.set_value(value)
    }

    fn cast_of(mut self, operand: Self::Operand, target: CastKind) {
        let cast_value: ValueRef = self
            .expr_builder()
            .cast(operand.into(), target, self.dest.type_info().clone())
            .into();

        self.set(cast_value)
    }

    fn binary_op_between(
        mut self,
        operator: crate::abs::BinaryOp,
        first: Self::Operand,
        second: Self::Operand,
    ) {
        let result_value = self
            .expr_builder()
            .binary_op((first, second).into(), operator);
        self.set(result_value.into())
    }

    fn unary_op_on(mut self, operator: UnaryOp, operand: Self::Operand) {
        let result_value = self.expr_builder().unary_op(operand.into(), operator);
        self.set(result_value.into())
    }

    /// # Remarks
    /// The tag place for the basic backend should be
    fn discriminant_from(
        mut self,
        TagPlaceWithInfo(tag_place, tag_info): Self::DiscriminablePlace,
    ) {
        let tag_value = self.vars_state.copy_place(&tag_place);
        let discr_value = if tag_value.is_symbolic() {
            self.build_discriminant_expr(
                SymValueRef::new(tag_value),
                &tag_info.encoding,
                self.dest.type_info(),
                tag_place.type_info(),
            )
            .into()
        } else {
            tag_value
        };
        self.set(discr_value)
    }

    fn array_from(mut self, elements: impl Iterator<Item = Self::Operand>) {
        let mut has_symbolic = false;
        let value = ConcreteValue::Array(ArrayValue {
            elements: elements
                .inspect(|e| has_symbolic |= e.is_symbolic())
                .collect(),
        });
        let value = if has_symbolic {
            value.into()
        } else {
            UnevalValue::Some.into()
        };
        self.set_value(value)
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
        self.set_adt_value(kind, fields.map(|f| Some(f)))
    }

    fn union_from(mut self, active_field: abs::FieldIndex, value: Self::Field) {
        let fields = (0..active_field)
            .map(|_| None)
            .chain(iter::once(Some(value)));
        self.set_adt_value(AdtKind::Struct, fields.into_iter())
    }

    fn raw_ptr_from(self, data_ptr: Self::Operand, metadata: Self::Operand, _is_mutable: bool) {
        self.adt_from([data_ptr, metadata].into_iter(), None)
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

    fn shallow_init_box_from(self, value: Self::Operand) {
        /* According to the Rust MIR documentation:
         * https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/enum.Rvalue.html#variant.ShallowInitBox
         * > Transmutes a *mut u8 into shallow-initialized Box<T>.
         * BTW, very improbable to have a symbolic value here. */
        let dst_ty_id = self.dest.type_info().id().unwrap();
        self.cast_of(value, CastKind::Transmute(dst_ty_id));
    }

    fn use_if_eq(mut self, val: Self::Operand, current: Self::Operand, expected: Self::Operand) {
        let are_eq = self
            .expr_builder()
            .eq((current.clone(), expected.clone()).into());
        let are_eq = if !are_eq.is_symbolic() {
            let current =
                ConcreteValueRef::new(current.clone()).try_resolve_as_const(self.type_manager);
            let expected = ConcreteValueRef::new(expected).try_resolve_as_const(self.type_manager);
            match (current, expected) {
                (Some(current), Some(expected)) => {
                    let are_eq = current == expected;
                    ConstValue::Bool(are_eq).to_value_ref()
                }
                _ => are_eq,
            }
        } else {
            are_eq
        };
        let value = self.expr_builder().if_then_else((are_eq, val, current));
        self.set(value)
    }

    fn use_and_check_eq(mut self, val: Self::Operand, expected: Self::Operand) {
        let are_eq = self.expr_builder().eq((val.clone(), expected).into());
        self.set_adt_value(AdtKind::Struct, [Some(val), Some(are_eq)].into_iter())
    }
}

impl<EB: OperationalExprBuilder> BasicAssignmentHandler<'_, EB> {
    #[inline]
    fn set(&mut self, value: ValueRef) {
        self.vars_state.set_place(&self.dest, value);
    }

    #[inline]
    fn set_value(&mut self, value: Value) {
        self.set(value.to_value_ref());
    }

    fn expr_builder(&self) -> impl DerefMut<Target = EB> + '_ {
        self.expr_builder.as_ref().borrow_mut()
    }

    fn set_adt_value(
        &mut self,
        kind: AdtKind,
        fields: impl Iterator<Item = Option<<Self as AssignmentHandler>::Field>>,
    ) {
        let mut has_symbolic = false;
        let fields = fields
            .map(|f| AdtField {
                value: f.inspect(|f| has_symbolic |= f.is_symbolic()),
            })
            .collect();
        let value = if has_symbolic {
            ConcreteValue::Adt(AdtValue { kind, fields }).into()
        } else {
            UnevalValue::Some.into()
        };
        self.set_value(value)
    }

    #[tracing::instrument(level = "debug", skip(self))]
    fn build_discriminant_expr(
        &self,
        tag_value: SymValueRef,
        tag_encoding: &common::tyexp::TagEncodingInfo,
        discr_ty_info: &LazyTypeInfo,
        tag_ty_info: &LazyTypeInfo,
    ) -> SymValueRef {
        use common::tyexp::TagEncodingInfo::*;
        match tag_encoding {
            Direct => tag_value,
            Niche {
                non_niche_value,
                niche_value_range,
                tag_value_start,
            } => {
                impl ValueType {
                    #[inline]
                    fn expect_int(&self) -> IntType {
                        match self {
                            ValueType::Int(ty) => *ty,
                            _ => {
                                // https://doc.rust-lang.org/reference/type-layout.html#primitive-representations
                                panic!(
                                    "Expected the type of the tag to be a int type. Found: {:?}",
                                    self
                                )
                            }
                        }
                    }
                }

                let get_int_type = |ty_info: &LazyTypeInfo| {
                    ValueType::try_from(ty_info)
                        .ok()
                        .unwrap_or_else(|| {
                            self.type_manager
                        .get_type(ty_info.id().unwrap())
                        .try_into()
                        .expect("Expected the type of the discriminant raw value to be a primitive")
                        })
                        .expect_int()
                };

                let discr_ty = get_int_type(discr_ty_info);
                let tag_ty = get_int_type(tag_ty_info);

                let into_tag_value = |v: u128| ConstValue::new_int(v, tag_ty).to_value_ref();
                let into_discr_value = |v: u128| ConstValue::new_int(v, discr_ty).to_value_ref();

                // Based on: `rustc_codegen_ssa::mir::place::PlaceRef::codegen_get_discr`
                let niche_start = *niche_value_range.start();
                let tag_value: ValueRef = tag_value.into();
                let relative_max = niche_value_range.end() - niche_start;
                let (is_niche, tagged_discr) = if relative_max == 0 {
                    let is_niche = SymValueRef::new(
                        self.expr_builder()
                            .eq((tag_value.clone(), into_tag_value(*tag_value_start)).into())
                            .into(),
                    );
                    let tagged_discr = into_discr_value(niche_start);
                    (is_niche, tagged_discr)
                } else {
                    let relative_tag_value: ValueRef = self
                        .expr_builder()
                        .sub((tag_value.clone(), into_tag_value(*tag_value_start)).into())
                        .into();
                    let is_niche = SymValueRef::new(
                        self.expr_builder()
                            .le((relative_tag_value.clone(), into_tag_value(relative_max)).into())
                            .into(),
                    );
                    let relative_discr_value = self
                        .expr_builder()
                        .to_int(relative_tag_value.into(), discr_ty, discr_ty_info.clone())
                        .into();
                    let tagged_discr = self
                        .expr_builder()
                        .add((relative_discr_value, into_discr_value(niche_start)).into())
                        .into();
                    let tagged_discr =
                        self.expr_builder()
                            .to_int(tagged_discr, discr_ty, discr_ty_info.clone());
                    debug_assert!(tagged_discr.is_symbolic());
                    (is_niche, tagged_discr)
                };

                let discr_value = self.expr_builder().if_then_else((
                    is_niche.into(),
                    tagged_discr.into(),
                    into_discr_value(*non_niche_value),
                ));
                SymValueRef::new(discr_value)
            }
        }
    }
}

pub(crate) struct BasicBranchingHandler<'a, EB: BinaryExprBuilder> {
    trace_manager: RefMut<'a, TraceManager>,
    expr_builder: RRef<EB>,
}

impl<'a> BasicBranchingHandler<'a, BasicExprBuilder> {
    fn new(backend: &'a mut BasicBackend) -> Self {
        Self {
            trace_manager: backend.trace_manager.borrow_mut(),
            expr_builder: backend.expr_builder.clone(),
        }
    }
}

impl<'a, EB: BinaryExprBuilder> BranchingHandler for BasicBranchingHandler<'a, EB> {
    type Operand = ValueRef;
    type ConditionalBranchingHandler = BasicConditionalBranchingHandler<'a, EB>;

    fn conditional(
        self,
        discriminant: Self::Operand,
        metadata: abs::BranchingMetadata,
    ) -> Self::ConditionalBranchingHandler {
        BasicConditionalBranchingHandler::new(
            discriminant,
            metadata,
            self.trace_manager,
            self.expr_builder,
        )
    }

    /// This function provides runtime support for all 5 assertion kinds in the leaf compiler.
    /// See: https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/mir/enum.AssertKind.html
    fn assert(
        mut self,
        cond: Self::Operand,
        expected: bool,
        _assert_kind: AssertKind<Self::Operand>,
    ) {
        // For now, we will call this function before the assert occurs and assume that assertions always succeed.
        // TODO: add a result: bool parameter to this function, and add support for it using a panic hook.
        if cond.is_symbolic() {
            // NOTE: This is a trick to pass the value through the expression builder
            // to ensure value resolving and simplifications.
            let expr = self
                .expr_builder
                .borrow_mut()
                .and((ConstValue::Bool(true).to_value_ref(), cond.clone()).into());
            let mut constraint = Constraint::Bool(expr.into());
            if !expected {
                constraint = constraint.not();
            }

            self.trace_manager.notify_step(
                0, /* TODO: The unique index of the block we have entered. */
                vec![constraint],
            );
        }
    }
}

pub(crate) struct BasicConditionalBranchingHandler<'a, EB: BinaryExprBuilder> {
    discriminant: ValueRef,
    metadata: BranchingMetadata,
    trace_manager: RefMut<'a, TraceManager>,
    expr_builder: RRef<EB>,
}

impl<'a, EB: BinaryExprBuilder> BasicConditionalBranchingHandler<'a, EB> {
    fn new(
        discriminant: ValueRef,
        metadata: BranchingMetadata,
        trace_manager: RefMut<'a, TraceManager>,
        expr_builder: RRef<EB>,
    ) -> Self {
        Self {
            discriminant,
            metadata,
            trace_manager,
            expr_builder,
        }
    }

    fn notify_constraint(&mut self, constraint: Constraint) {
        self.trace_manager.notify_step(
            0, /* TODO: The unique index of the block we have entered. */
            vec![constraint],
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

        // NOTE: This is a trick to pass the value through the expression builder
        // to ensure value resolving and simplifications.
        let expr = self
            .expr_builder()
            .and(
                (
                    ConstValue::Bool(true).to_value_ref(),
                    self.parent.discriminant.clone(),
                )
                    .into(),
            )
            .into();
        let mut constraint = Constraint::Bool(expr);
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

                    let expr = self.create_equality_expr(value, true);
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
    fn new(backend: &'a mut BasicBackend) -> Self {
        Self {
            call_stack_manager: &mut backend.call_stack_manager,
            type_manager: backend.type_manager.as_ref(),
        }
    }
}

impl<'a> FunctionHandler for BasicFunctionHandler<'a> {
    type Place = PlaceValueRef;
    type Operand = ValueRef;
    type MetadataHandler = ();

    #[inline]
    fn before_call(
        self,
        func: Self::Operand,
        args: impl Iterator<Item = Self::Arg>,
        are_args_tupled: bool,
    ) {
        self.call_stack_manager
            .prepare_for_call(func, args.collect(), are_args_tupled);
    }

    fn enter(
        self,
        func: Self::Operand,
        arg_places: impl Iterator<Item = Self::Place>,
        ret_val_place: Self::Place,
        tupled_arg: Option<(Local, TypeId)>,
    ) {
        fn ensure_deter_place(place: PlaceValueRef) -> DeterPlaceValueRef {
            debug_assert!(!place.is_symbolic());
            DeterPlaceValueRef::new(place)
        }
        self.call_stack_manager.set_places(
            arg_places.map(ensure_deter_place).collect(),
            ensure_deter_place(ret_val_place),
        );
        if let Some((arg_index, tuple_type_id)) = tupled_arg {
            let Local::Argument(arg_index) = arg_index else {
                unreachable!()
            };
            self.call_stack_manager
                .try_untuple_argument(arg_index, &|| {
                    Box::new(BasicUntupleHelper::new(self.type_manager, tuple_type_id))
                });
        }
        self.call_stack_manager.notify_enter(func);
    }

    #[inline]
    fn override_return_value(self, value: Self::Operand) {
        self.call_stack_manager.override_return_value(value)
    }

    #[inline]
    fn ret(self) {
        self.call_stack_manager.pop_stack_frame();
    }

    fn after_call(self, result_dest: Self::Place) {
        debug_assert!(!result_dest.is_symbolic());
        self.call_stack_manager
            .finalize_call(DeterPlaceValueRef::new(result_dest));
    }

    fn metadata(self) -> Self::MetadataHandler {
        Default::default()
    }
}

struct BasicUntupleHelper<'a> {
    type_manager: &'a dyn TypeManager,
    tuple_type_id: TypeId,
    type_info: Option<&'static TypeInfo>,
    fields_info: Option<&'static StructShape>,
}

impl GenericUntupleHelper for BasicUntupleHelper<'_> {
    type PlaceInfo = Place;
    type Place = DeterPlaceValueRef;

    fn make_tupled_arg_pseudo_place(&mut self, addr: RawAddress) -> Self::Place {
        DeterPlaceValueRef::new(
            DeterministicPlaceValue::from_addr_type(addr, self.tuple_type_id).to_value_ref(),
        )
    }

    fn num_fields(&mut self) -> FieldIndex {
        self.type_info()
            .expect_single_variant()
            .fields
            .as_struct()
            .unwrap()
            .fields
            .len() as FieldIndex
    }

    fn field_place(&mut self, base: Self::Place, field: FieldIndex) -> Self::Place {
        let field_info = &self.fields_info().fields[field as usize];
        DeterPlaceValueRef::new(
            DeterministicPlaceValue::from_addr_type(
                base.address().wrapping_byte_add(field_info.offset as usize),
                field_info.ty,
            )
            .to_value_ref(),
        )
    }
}

impl<'a> BasicUntupleHelper<'a> {
    fn new(type_manager: &'a dyn TypeManager, tuple_type_id: TypeId) -> Self {
        Self {
            type_manager,
            tuple_type_id,
            type_info: None,
            fields_info: None,
        }
    }

    #[inline]
    fn get_type(&self, type_id: TypeId) -> &'static TypeInfo {
        self.type_manager.get_type(type_id)
    }

    fn type_info(&mut self) -> &'static TypeInfo {
        if self.type_info.is_none() {
            self.type_info = Some(self.get_type(self.tuple_type_id));
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
}

type Constraint = crate::abs::Constraint<ValueRef>;

pub(crate) struct TagPlaceWithInfo(PlaceValueRef, &'static common::tyexp::TagInfo);

trait GenericVariablesState {
    type PlaceInfo = Place;
    type PlaceValue = PlaceValueRef;
    type Value = ValueRef;

    fn id(&self) -> usize;

    /// Returns a value that corresponds to the place itself.
    /// The returned value does not necessarily access the actual value but
    /// should be dereferenceable to get the actual value.
    fn ref_place(&self, place: &Self::PlaceInfo, usage: PlaceUsage) -> Self::PlaceValue;

    /// Returns a value that corresponds to the place pointer by the pointer.
    /// Effectively, this is equivalent to the place that would be represented by `*ptr`.
    fn ref_place_by_ptr(
        &self,
        ptr: Self::Value,
        ptr_type_id: TypeId,
        usage: PlaceUsage,
    ) -> Self::PlaceValue;

    /// Returns a copy of the value stored at the given place. May not physically copy the value
    /// but the returned value should be independently usable from the original value.
    fn copy_place(&self, place: &Self::PlaceValue) -> Self::Value;

    /// Returns the value stored at the given place.
    /// Conceptually, it is required that the place will not contain the value right after this operation.
    fn take_place(&mut self, place: &Self::PlaceValue) -> Self::Value;

    /// Sets the value of a place. Overwrites the previous value if any, also defines a new local
    /// variable if it does not exist.
    fn set_place(&mut self, place: &Self::PlaceValue, value: Self::Value);
}

trait VariablesState:
    GenericVariablesState<PlaceInfo = Place, PlaceValue = PlaceValueRef, Value = ValueRef>
{
}
impl<T> VariablesState for T where
    T: GenericVariablesState<PlaceInfo = Place, PlaceValue = PlaceValueRef, Value = ValueRef>
{
}

trait GenericCallStackManager {
    type VariablesState: GenericVariablesState;
    type Place = <Self::VariablesState as GenericVariablesState>::PlaceValue;
    type Value = <Self::VariablesState as GenericVariablesState>::Value;

    /* NOTE: Why `are_args_tupled` are passed? Isn't `try_untuple_argument` enough?
     * First, arguments are tupled at the call site, which also calls this function.
     * Second, when untupling, we should make sure that the arguments were tupled.
     * If closures are converted to a function pointer, then the arguments are not tupled.
     */
    fn prepare_for_call(
        &mut self,
        func: Self::Value,
        args: Vec<Self::Value>,
        are_args_tupled: bool,
    );

    fn set_places(&mut self, arg_places: Vec<Self::Place>, ret_val_place: Self::Place);

    fn try_untuple_argument<'a, 'b>(
        &'a mut self,
        arg_index: LocalIndex,
        untuple_helper: &dyn Fn() -> Box<dyn UntupleHelper + 'b>,
    );

    fn notify_enter(&mut self, current_func: Self::Value);

    fn pop_stack_frame(&mut self);

    fn override_return_value(&mut self, value: Self::Value);

    fn finalize_call(&mut self, result_dest: Self::Place);

    fn top(&mut self) -> &mut Self::VariablesState;
}

trait GenericUntupleHelper {
    type PlaceInfo;
    type Place;

    fn make_tupled_arg_pseudo_place(&mut self, addr: RawAddress) -> Self::Place;

    fn num_fields(&mut self) -> FieldIndex;

    /// Takes a place and returns a place with projection to the field.
    /// It should make a valid place with full information needed for the state.
    fn field_place(&mut self, base: Self::Place, field: FieldIndex) -> Self::Place;
}

trait UntupleHelper: GenericUntupleHelper<PlaceInfo = Place, Place = DeterPlaceValueRef> {}
impl<T> UntupleHelper for T where
    T: GenericUntupleHelper<PlaceInfo = Place, Place = DeterPlaceValueRef>
{
}

trait CallStackManager:
    GenericCallStackManager<
        VariablesState = BasicVariablesState,
        Place = DeterPlaceValueRef,
        Value = ValueRef,
    >
{
}
impl<T> CallStackManager for T where
    T: GenericCallStackManager<
            VariablesState = BasicVariablesState,
            Place = DeterPlaceValueRef,
            Value = ValueRef,
        >
{
}
