pub(crate) mod expr;
pub(crate) mod logger;
pub(crate) mod operand;
pub(crate) mod place;

use std::{collections::HashMap, mem};

use crate::{
    abs::{
        self, backend::*, AssertKind, BasicBlockIndex, BinaryOp, BranchingMetadata,
        DiscriminantAsIntType, FieldIndex, Local, UnaryOp, VariantIndex,
    },
    solvers::z3::Z3Solver,
    trace::ImmediateTraceManager,
};

use self::{
    expr::{
        AdtKind, AdtValue, ArrayValue, ConcreteValue, ConstValue, Expr, RefValue, SymValue,
        SymValueRef, SymbolicVar, Value,
    },
    operand::{DefaultOperandHandler, Operand},
    place::{DefaultPlaceHandler, Place, Projection},
};

type TraceManager = Box<dyn abs::backend::TraceManager<BasicBlockIndex, ValueRef>>;

pub struct BasicBackend {
    call_stack_manager: CallStackManager,
    trace_manager: TraceManager,
    current_constraints: Vec<Constraint>,
}

impl BasicBackend {
    pub fn new() -> Self {
        Self {
            call_stack_manager: CallStackManager::new(),
            trace_manager: Box::new(
                ImmediateTraceManager::<BasicBlockIndex, ValueRef, u32>::new_basic(Box::new(
                    Z3Solver::new_in_global_context(),
                )),
            ),
            current_constraints: Vec::new(),
        }
    }
}

impl RuntimeBackend for BasicBackend {
    type PlaceHandler<'a> = DefaultPlaceHandler
    where
        Self: 'a;

    type OperandHandler<'a> = DefaultOperandHandler
    where
        Self: 'a;

    type AssignmentHandler<'a> = BasicAssignmentHandler<'a>
    where
        Self: 'a;

    type BranchingHandler<'a> = BasicBranchingHandler<'a>
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
        DefaultOperandHandler
    }

    fn assign_to<'a>(
        &'a mut self,
        dest: <Self::AssignmentHandler<'a> as AssignmentHandler>::Place,
    ) -> Self::AssignmentHandler<'a> {
        BasicAssignmentHandler::new(dest, self.current_vars_state())
    }

    fn branch<'a>(&'a mut self) -> Self::BranchingHandler<'a> {
        BasicBranchingHandler {
            call_stack_manager: &mut self.call_stack_manager,
            trace_manager: &mut self.trace_manager,
            current_constraints: &mut self.current_constraints,
        }
    }

    fn func_control(&mut self) -> Self::FunctionHandler<'_> {
        BasicFunctionHandler::new(&mut self.call_stack_manager)
    }
}

impl BasicBackend {
    fn current_vars_state(&mut self) -> &mut MutableVariablesState {
        self.call_stack_manager.top()
    }
}

pub(crate) struct BasicAssignmentHandler<'s> {
    dest: Place,
    vars_state: &'s mut MutableVariablesState,
}

impl<'s> BasicAssignmentHandler<'s> {
    fn new(dest: Place, vars_state: &'s mut MutableVariablesState) -> Self {
        Self { dest, vars_state }
    }
}

impl AssignmentHandler for BasicAssignmentHandler<'_> {
    type Place = Place;
    type Operand = Operand;

    fn use_of(mut self, operand: Self::Operand) {
        let value = self.get_operand_value(&operand);
        self.set(value)
    }

    fn repeat_of(mut self, operand: Self::Operand, count: usize) {
        let element_value = self.get_operand_value(&operand);
        /* NOTE: As we don't expect the count to be a large number, we currently,
         * don't optimize this by using a single element and a length.
         */
        let value = Value::Concrete(ConcreteValue::Array(ArrayValue {
            elements: vec![element_value; count],
        }));
        self.set_value(value)
    }

    fn ref_to(mut self, place: Self::Place, is_mutable: bool) {
        self.set_value(Value::Concrete(ConcreteValue::Ref(if is_mutable {
            RefValue::Mut(place)
        } else {
            RefValue::Immut(self.vars_state.copy_place(&place))
        })))
    }

    fn thread_local_ref_to(self) {
        todo!()
    }

    fn address_of(self, _place: Self::Place, _is_mutable: bool) {
        todo!()
    }

    fn len_of(mut self, place: Self::Place) {
        let value = self.vars_state.copy_place(&place);
        let len_value = match value.as_ref() {
            Value::Concrete(ConcreteValue::Array(arr)) => {
                let len = arr.len();
                Value::Concrete(ConcreteValue::from_const(ConstValue::Int {
                    bit_rep: len as u128,
                    size: mem::size_of_val(&len) as u64 * 8,
                    is_signed: false,
                }))
            }
            _ => panic!("Length is supposed to be called on a (concrete) array."),
        };
        self.set_value(len_value)
    }

    fn char_cast_of(mut self, operand: Self::Operand) {
        let value = self.get_operand_value(&operand);

        if value.is_symbolic() {
            return self.set_value(Value::Symbolic(SymValue::Expression(Expr::Cast {
                from: SymValueRef::new(value),
                to: expr::SymbolicVarType::Char,
            })));
        }

        let value = match value.as_ref() {
            Value::Concrete(ConcreteValue::Const(ConstValue::Int { bit_rep, .. })) => {
                let char_value = *bit_rep as u8 as char;
                Value::Concrete(ConcreteValue::from_const(ConstValue::Char(char_value)))
            }
            _ => panic!("Char cast is supposed to be called on a (concrete) integer."),
        };

        self.set_value(value)
    }

    fn integer_cast_of(mut self, operand: Self::Operand, is_signed: bool, bits: u64) {
        let value = self.get_operand_value(&operand);

        if value.is_symbolic() {
            return self.set_value(Value::Symbolic(SymValue::Expression(Expr::Cast {
                from: SymValueRef::new(value),
                to: expr::SymbolicVarType::Int {
                    is_signed,
                    size: bits,
                },
            })));
        }

        let value = match value.as_ref() {
            Value::Concrete(ConcreteValue::Const(c)) => {
                Value::from_const(ConstValue::integer_cast(c, bits, is_signed))
            }
            _ => unreachable!("Integer cast is supposed to be called on a (concrete) constant."),
        };

        self.set_value(value)
    }

    fn float_cast_of(self, _operand: Self::Operand, _bits: u64) {
        todo!()
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

        let first_value = self.get_operand_value(&first);
        let second_value = self.get_operand_value(&second);
        let pair = if first_value.is_symbolic() {
            (first_value, second_value, false)
        } else {
            (second_value, first_value, true)
        };
        if pair.0.is_symbolic() {
            return self.set_value(Value::Symbolic(SymValue::Expression(Expr::Binary {
                operator,
                first: SymValueRef::new(pair.0),
                second: pair.1,
                is_flipped: pair.2,
            })));
        }

        let value = match (pair.0.as_ref(), pair.1.as_ref()) {
            (
                Value::Concrete(ConcreteValue::Const(first_c)),
                Value::Concrete(ConcreteValue::Const(second_c)),
            ) => Value::from_const(ConstValue::binary_op(first_c, second_c, operator)),
            _ => unreachable!("Binary operations are only supposed to be called on constants."),
        };
        self.set_value(value)
    }

    fn unary_op_on(mut self, operator: UnaryOp, operand: Self::Operand) {
        let value = self.get_operand_value(&operand);
        if value.is_symbolic() {
            return self.set_value(Value::Symbolic(SymValue::Expression(Expr::Unary {
                operator,
                operand: SymValueRef::new(value),
            })));
        }

        let value = match value.as_ref() {
            Value::Concrete(ConcreteValue::Const(c)) => {
                Value::from_const(ConstValue::unary_op(c, operator))
            }
            _ => unreachable!("Unary operations are only supposed to be called on constants."),
        };
        self.set_value(value)
    }

    fn discriminant_of(mut self, place: Self::Place) {
        let host_value = self.vars_state.get_place(&place);
        match host_value.as_ref() {
            Value::Concrete(ConcreteValue::Adt(AdtValue {
                kind: AdtKind::Enum {
                    discriminant: discr,
                },
                ..
            })) => self.set_value(Value::from_const(ConstValue::from(*discr))),
            _ => unreachable!("Discriminant is only supposed to be called on (concrete) enums."),
        }
    }

    fn array_from(mut self, items: impl Iterator<Item = Self::Operand>) {
        let value = Value::Concrete(ConcreteValue::Array(ArrayValue {
            elements: items.map(|e| self.get_operand_value(&e)).collect(),
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

impl BasicAssignmentHandler<'_> {
    fn set(&mut self, value: ValueRef) {
        self.vars_state.set_place(&self.dest, value);
    }

    fn set_value(&mut self, value: Value) {
        self.set(ValueRef::new(value));
    }

    fn get_operand_value(
        &mut self,
        operand: &<BasicAssignmentHandler as AssignmentHandler>::Operand,
    ) -> ValueRef {
        get_operand_value(self.vars_state, operand)
    }
}

pub(crate) struct BasicBranchingHandler<'a> {
    call_stack_manager: &'a mut CallStackManager,
    trace_manager: &'a mut TraceManager,
    current_constraints: &'a mut Vec<Constraint>,
}

impl<'a> BranchingHandler for BasicBranchingHandler<'a> {
    type Operand = Operand;
    type ConditionalBranchingHandler = BasicConditionalBranchingHandler<'a>;

    fn conditional(
        self,
        discriminant: Operand,
        metadata: abs::BranchingMetadata,
    ) -> Self::ConditionalBranchingHandler {
        let disc = get_operand_value(self.call_stack_manager.top(), &discriminant);
        BasicConditionalBranchingHandler::new(
            disc,
            metadata,
            self.trace_manager,
            self.current_constraints,
        )
    }

    fn assert(self, cond: Self::Operand, expected: bool, assert_kind: AssertKind<Self::Operand>) {
        todo!("implement logic for assertions")
    }
}

pub(crate) struct BasicConditionalBranchingHandler<'a> {
    discriminant: ValueRef,
    metadata: BranchingMetadata,
    trace_manager: &'a mut TraceManager,
    current_constraints: &'a mut Vec<Constraint>,
}

impl<'a> BasicConditionalBranchingHandler<'a> {
    fn new(
        discriminant: ValueRef,
        metadata: BranchingMetadata,
        trace_manager: &'a mut TraceManager,
        current_constraints: &'a mut Vec<Constraint>,
    ) -> Self {
        Self {
            discriminant,
            metadata,
            trace_manager,
            current_constraints,
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

impl<'a> ConditionalBranchingHandler for BasicConditionalBranchingHandler<'a> {
    type BoolBranchTakingHandler = BasicBranchTakingHandler<'a>;
    type IntBranchTakingHandler = BasicBranchTakingHandler<'a>;
    type CharBranchTakingHandler = BasicBranchTakingHandler<'a>;
    type EnumBranchTakingHandler = BasicBranchTakingHandler<'a>;

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

pub(crate) struct BasicBranchTakingHandler<'a> {
    parent: BasicConditionalBranchingHandler<'a>,
}

impl BasicBranchTakingHandler<'_> {
    fn create_equality_expr(&self, value: impl BranchCaseValue, eq: bool) -> Expr {
        let discr_as_int = &self.parent.metadata.discr_as_int;
        Expr::Binary {
            operator: if eq { BinaryOp::Eq } else { BinaryOp::Ne },
            first: SymValueRef::new(self.parent.discriminant.clone()),
            second: ValueRef::new(Value::from_const(value.into_const(discr_as_int))),
            is_flipped: false,
        }
    }
}

impl BranchTakingHandler<bool> for BasicBranchTakingHandler<'_> {
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
            impl BranchTakingHandler<$type> for BasicBranchTakingHandler<'_> {
                fn take(mut self, value: $type) {
                    if !self.parent.discriminant.is_symbolic() {
                        return;
                    }

                    let expr = self.create_equality_expr(value as u128, true);
                    let constraint = Constraint::Bool(expr.into_value_ref().0);
                    self.parent.notify_constraint(constraint);
                }

                fn take_otherwise(mut self, non_values: &[$type]) {
                    if !self.parent.discriminant.is_symbolic() {
                        return;
                    }

                    /* Converting all non-equalities into a single constraint to not lose
                    * semantics.
                    */
                    let constraint = Constraint::Bool(
                        non_values
                            .into_iter()
                            .map(|v| self.create_equality_expr(*v, false))
                            .reduce(|acc, e| Expr::Binary {
                                operator: BinaryOp::BitAnd,
                                first: acc.into_value_ref(),
                                second: e.into_value_ref().0,
                                is_flipped: false,
                            })
                            .unwrap()
                            .into_value_ref()
                            .0,
                    );
                    self.parent.notify_constraint(constraint);
                }
            }
        )*
    };
}

impl_general_branch_taking_handler!(u128, char, VariantIndex);

trait BranchCaseValue {
    fn into_const(self, discr_as_int: &DiscriminantAsIntType) -> ConstValue;
}

impl BranchCaseValue for char {
    fn into_const(self, _discr_as_int: &DiscriminantAsIntType) -> ConstValue {
        ConstValue::Char(self)
    }
}

macro_rules! impl_int_branch_case_value {
    ($($type:ty),*) => {
        $(
            impl BranchCaseValue for $type {
                fn into_const(self, discr_as_int: &DiscriminantAsIntType) -> ConstValue {
                    ConstValue::Int {
                        bit_rep: self as u128,
                        size: discr_as_int.bit_size,
                        is_signed: discr_as_int.is_signed,
                    }
                }
            }
        )*
    };
}

impl_int_branch_case_value!(u128, VariantIndex);

pub(crate) struct BasicFunctionHandler<'a> {
    call_stack_manager: &'a mut CallStackManager,
}

impl<'a> BasicFunctionHandler<'a> {
    fn new(call_stack_manager: &'a mut CallStackManager) -> Self {
        Self { call_stack_manager }
    }
}

impl FunctionHandler for BasicFunctionHandler<'_> {
    type Place = Place;
    type Operand = Operand;

    fn before_call(self, _func: Self::Operand, args: impl Iterator<Item = Self::Operand>) {
        self.call_stack_manager.update_call_info(args.collect());
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

        if !self.call_stack_manager.is_empty() {
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
}

type ValueRef = expr::ValueRef;

struct MutableVariablesState {
    locals: HashMap<Local, ValueRef>,
}

impl MutableVariablesState {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
        }
    }

    pub fn copy_place(&self, place: &Place) -> ValueRef {
        self.get_place(place).clone()
    }

    pub fn take_place(&mut self, place: &Place) -> ValueRef {
        self.try_take_place(place)
            .unwrap_or_else(|| panic!("{}", Self::get_err_message(&place.local())))
    }

    pub fn try_take_place(&mut self, place: &Place) -> Option<ValueRef> {
        let local = place.local();
        let value = self.locals.remove(&local)?;

        let mut value = value;

        if !place.has_projection() {
            return Some(value);
        }

        let (last, projs) = place.projections.split_last().unwrap();
        let last = match last {
            Projection::Field(field) => *field,
            _ => panic!("Move can only happen on locals or partially move on struct fields."),
        };

        let field_val = Self::take_field(self.apply_projs_mut(&mut value, projs.iter()), last);
        self.locals.insert(local, value);
        Some(field_val)
    }

    pub fn set_place(&mut self, place: &Place, value: ValueRef) {
        let local = place.local();
        if !place.has_projection() {
            self.locals.insert(local, value);
        } else {
            self.mut_place(place, |_, v| *v = value);
        }
    }

    /// Takes an inner field from a struct value.
    /// If there are some projections left, it will recursively take the inner
    /// field while keeping the parent fields in place.
    ///
    /// # Returns
    /// The taken out inner field.
    ///
    /// # Panics
    ///
    /// Panics if the value is not a struct.
    fn take_field(value: &mut ValueRef, field: FieldIndex) -> ValueRef {
        match ValueRef::make_mut(value) {
            Value::Concrete(ConcreteValue::Adt(AdtValue {
                kind: AdtKind::Struct,
                ref mut fields,
            })) => fields[field as usize]
                .take()
                .unwrap_or_else(|| panic!("Field should not be moved before. {field}")),
            Value::Symbolic(_) => {
                unimplemented!("Partial move on symbolic values is not currently supported.")
            }
            _ => panic!("Only (concrete) structs can be partially moved."),
        }
    }

    fn get_place(&self, place: &Place) -> &ValueRef {
        self.get_place_iter(place.local(), place.projections.iter())
    }

    fn get_place_iter<'a, 'b>(
        &'a self,
        local: Local,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> &ValueRef
    where
        'a: 'b,
    {
        let host = &self
            .locals
            .get(&local)
            .unwrap_or_else(|| panic!("{}", Self::get_err_message(&local)));
        self.apply_projs(host, projs)
    }

    fn apply_projs<'a, 'b>(
        &'a self,
        host: &'a ValueRef,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> &'_ ValueRef
    where
        'a: 'b,
    {
        let mut current = host;
        for proj in projs {
            let concrete = match current.as_ref() {
                Value::Symbolic(_) => {
                    unimplemented!("Projectable symbolic values are not supported yet.")
                }
                Value::Concrete(concrete) => concrete,
            };
            let err = format!(
                "Projection {:?} is not possible on this value {:?}.",
                &proj, &concrete
            );
            current = match proj {
                Projection::Deref => match concrete {
                    ConcreteValue::Ref(RefValue::Immut(value)) => value,
                    ConcreteValue::Ref(RefValue::Mut(place)) => self.get_place(place),
                    _ => panic!("{err}"),
                },
                Projection::Field(field) => match concrete {
                    ConcreteValue::Adt(AdtValue { fields, .. }) => fields[*field as usize]
                        .as_ref()
                        .unwrap_or_else(|| panic!("Field should not be moved before. {field}")),
                    _ => panic!("{err}"),
                },
                Projection::Index(index) => match concrete {
                    ConcreteValue::Array(ArrayValue { elements }) => {
                        let index = self.copy_place(index);
                        match index.as_ref() {
                            Value::Concrete(ConcreteValue::Const(ConstValue::Int {
                                bit_rep,
                                ..
                            })) => &elements[*bit_rep as usize],
                            Value::Symbolic(_) => {
                                todo!("Symbolic array index is not supported yet.")
                            }
                            _ => panic!("Index should be an integer."),
                        }
                    }
                    _ => panic!("{err}"),
                },
                Projection::ConstantIndex {
                    offset, from_end, ..
                } => match concrete {
                    ConcreteValue::Array(ArrayValue { elements }) => {
                        let offset = *offset as usize;
                        if *from_end {
                            &elements[elements.len() - offset]
                        } else {
                            &elements[offset]
                        }
                    }
                    _ => panic!("{err}"),
                },
                Projection::Subslice { .. } => todo!(),
                Projection::Downcast(_) => todo!(),
                Projection::OpaqueCast => todo!(),
            };
        }
        current
    }

    fn mut_place(&mut self, place: &Place, mutate: impl FnOnce(&Self, &mut ValueRef)) {
        self.mut_place_iter(place.local(), &place.projections, mutate);
    }

    fn mut_place_iter(
        &mut self,
        local: Local,
        projs: &[Projection],
        mutate: impl FnOnce(&Self, &mut ValueRef),
    ) {
        // FIXME: Is there a way to avoid vector allocation?
        let mut projs = projs.to_vec();

        // Up to the last deref, we only need the place. So no mutable borrow is needed.
        while let Some(deref_index) = projs.iter().rposition(|p| matches!(p, Projection::Deref)) {
            let value = self.get_place_iter(local, projs.iter().take(deref_index));
            match value.as_ref() {
                Value::Concrete(ConcreteValue::Ref(RefValue::Mut(place))) => {
                    /* NOTE: We are taking this approach because recursively
                     * calling the function was leading to infinite recursion in
                     * the compiler's borrow checker.
                     */
                    projs.splice(..=deref_index, place.projections.iter().cloned());
                }
                _ => panic!("The last deref is expected to be on a mutable reference."),
            }
        }

        /* NOTE: As we are temporarily removing the local (to mitigate borrowing issues),
         * are we sure that the local is not used in the projections?
         * Yes. The recursive projection types are Deref and Index. The deref
         * cannot have reference to the same local, because it's against the
         * borrowing rules. The index also is on another local (not a place),
         * so the value will be in another local and not causing any issues.
         */
        let mut local_value = self
            .locals
            .remove(&local)
            .unwrap_or_else(|| panic!("{}", Self::get_err_message(&local)));
        let value = self.apply_projs_mut(&mut local_value, projs.iter());
        mutate(self, value);
        self.locals.insert(local, local_value);
    }

    fn apply_projs_mut<'a, 'b>(
        &'a self,
        host: &'a mut ValueRef,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> &'_ mut ValueRef
    where
        'a: 'b,
    {
        let mut current = host;
        for proj in projs {
            /* NOTE: Alive copies on projectable types does not seem to be possible.
             * For compound types, it is not observed in the MIR. Only immutable ref
             * can be copied, which is not possible to be given to this function.
             * Therefore, we expect that make_mut is not going to clone when there
             * are some projections.
             */
            let original_addr = ValueRef::as_ptr(current);
            let current_value = ValueRef::make_mut(current);
            debug_assert_eq!(original_addr, current_value);
            let concrete = match current_value {
                Value::Symbolic(_) => {
                    unimplemented!("Projectable symbolic values are not supported yet.")
                }
                Value::Concrete(concrete) => concrete,
            };
            let err = format!(
                "Projection {:?} is not possible on this value {:?}.",
                &proj, &concrete
            );
            current = match proj {
                Projection::Deref => {
                    panic!("Deref is not expected to be here. Consider calling get_place_mut.")
                }
                Projection::Field(field) => match concrete {
                    ConcreteValue::Adt(AdtValue { fields, .. }) => fields[*field as usize]
                        .as_mut()
                        .unwrap_or_else(|| panic!("Field should not be moved before. {field}")),
                    _ => panic!("{err}"),
                },
                Projection::Index(index) => match concrete {
                    ConcreteValue::Array(ArrayValue { elements }) => {
                        assert!(
                            !index.has_projection(),
                            "Index should be a simple local with no projection."
                        );
                        let index = self.get_place_iter(index.local(), std::iter::empty());
                        match index.as_ref() {
                            Value::Concrete(ConcreteValue::Const(ConstValue::Int {
                                bit_rep,
                                ..
                            })) => &mut elements[*bit_rep as usize],
                            Value::Symbolic(_) => {
                                todo!("Symbolic array index is not supported yet.")
                            }
                            _ => panic!("Index should be an integer."),
                        }
                    }
                    _ => panic!("{err}"),
                },
                Projection::ConstantIndex {
                    offset, from_end, ..
                } => match concrete {
                    ConcreteValue::Array(ArrayValue { elements }) => {
                        let offset = *offset as usize;
                        if *from_end {
                            let len = elements.len();
                            &mut elements[len - offset]
                        } else {
                            &mut elements[offset]
                        }
                    }
                    _ => panic!("{err}"),
                },
                Projection::Subslice { .. } => todo!(),
                Projection::Downcast(_) => todo!(),
                Projection::OpaqueCast => todo!(),
            };
        }
        current
    }

    #[inline]
    fn get_err_message(local: &Local) -> String {
        format!("Uninitialized, moved, or invalid local. {local}")
    }
}

type Constraint = crate::abs::Constraint<ValueRef>;

struct CallStackManager {
    stack: Vec<CallStackFrame>,
    current_call_info: Vec<Operand>,
    returned_val: Option<ValueRef>,
    is_external_function: bool,
}

struct CallStackFrame {
    vars_state: MutableVariablesState,
}

impl CallStackManager {
    fn new() -> Self {
        Self {
            stack: vec![],
            current_call_info: vec![],
            returned_val: None,
            is_external_function: true,
        }
    }

    fn update_call_info(&mut self, call_info: Vec<Operand>) {
        self.current_call_info = call_info;
    }

    fn push_stack_frame(&mut self) {
        let mut vars_state = MutableVariablesState::new();

        // set places for the arguments in the new frame using values from the current frame
        let operands: Vec<Operand> = self.current_call_info.drain(..).collect();
        for (i, operand) in operands.iter().enumerate() {
            let value = get_operand_value(self.top(), &operand);
            let local_index = (i + 1) as u32;
            let place = &Place::new(Local::Argument(local_index));
            vars_state.set_place(place, value);
        }

        self.stack.push(CallStackFrame { vars_state });
    }

    fn pop_stack_frame(&mut self) {
        self.returned_val = self.top().try_take_place(&Place::new(Local::ReturnValue));
        self.is_external_function = false;
        self.stack.pop();
    }

    fn get_return_info(&mut self) -> (Option<ValueRef>, bool) {
        let returned_val = self.returned_val.clone();
        let is_external_function = self.is_external_function;

        self.returned_val = None;
        self.is_external_function = true;

        (returned_val, is_external_function)
    }

    fn top(&mut self) -> &mut MutableVariablesState {
        &mut self
            .stack
            .last_mut()
            .expect("Call stack is empty.")
            .vars_state
    }

    fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }
}

fn get_operand_value(vars_state: &mut MutableVariablesState, operand: &Operand) -> ValueRef {
    match operand {
        Operand::Place(place, operand::PlaceUsage::Copy) => vars_state.copy_place(place),
        Operand::Place(place, operand::PlaceUsage::Move) => vars_state.take_place(place),
        Operand::Const(constant) => ValueRef::new(Value::Concrete(ConcreteValue::from_const(
            get_constant_value(constant),
        ))),
        Operand::Symbolic(sym) => ValueRef::new(Value::Symbolic(SymValue::Variable(
            SymbolicVar::new(0, sym.into()),
        ))),
    }
}

fn get_constant_value(constant: &operand::Constant) -> ConstValue {
    match constant {
        operand::Constant::Bool(value) => ConstValue::Bool(*value),
        operand::Constant::Char(value) => ConstValue::Char(*value),
        operand::Constant::Int {
            bit_rep,
            size,
            is_signed,
        } => ConstValue::Int {
            bit_rep: *bit_rep,
            size: *size,
            is_signed: *is_signed,
        },
        operand::Constant::Float {
            bit_rep,
            ebits,
            sbits,
        } => ConstValue::Float {
            bit_rep: *bit_rep,
            ebits: *ebits,
            sbits: *sbits,
        },
        operand::Constant::Str(value) => ConstValue::Str(value),
        operand::Constant::Func(value) => ConstValue::Func(*value),
    }
}
