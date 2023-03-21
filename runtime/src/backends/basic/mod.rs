use std::{collections::HashMap, mem, rc::Rc};

use crate::abs::{
    AssignmentHandler, BasicBlockIndex, BinaryOp, BranchTakingHandler, BranchingHandler,
    FieldIndex, FunctionHandler, Local, RuntimeBackend, UnaryOp, VariantIndex,
};

use self::{
    expr::{
        AdtKind, AdtValue, ArrayValue, ConcreteValue, ConstValue, Expr, RefValue, SymValue,
        SymValueRef, Value,
    },
    operand::{DefaultOperandHandler, Operand},
    place::{DefaultPlaceHandler, Place, Projection},
};

pub(crate) mod expr;

pub(crate) mod operand;
pub(crate) mod place;

pub(crate) mod logger;

pub struct BasicBackend {
    constraint_manager: ConstraintManager,
    call_stack_manager: CallStackManager,
}

impl BasicBackend {
    pub fn new() -> Self {
        Self {
            constraint_manager: ConstraintManager::new(),
            call_stack_manager: CallStackManager::new(),
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

    fn place<'a>(&'a mut self) -> Self::PlaceHandler<'a> {
        DefaultPlaceHandler
    }

    fn operand<'a>(&'a mut self) -> Self::OperandHandler<'a> {
        DefaultOperandHandler
    }

    fn assign_to<'a>(
        &'a mut self,
        dest: <Self::AssignmentHandler<'a> as crate::abs::AssignmentHandler>::Place,
    ) -> Self::AssignmentHandler<'a> {
        BasicAssignmentHandler::new(dest, self.current_vars_state())
    }

    fn branch<'a>(
        &'a mut self,
        location: crate::abs::BasicBlockIndex,
        discriminant: <Self::OperandHandler<'static> as crate::abs::OperandHandler>::Operand,
    ) -> Self::BranchingHandler<'a> {
        BasicBranchingHandler::new(
            location,
            get_operand_value(self.current_vars_state(), &discriminant),
            &mut self.constraint_manager,
        )
    }

    fn func_control<'a>(&'a mut self) -> Self::FunctionHandler<'a> {
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

    fn address_of(self, place: Self::Place, is_mutable: bool) {
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

    fn numeric_cast_of(self, operand: Self::Operand, is_to_float: bool, size: usize) {
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
        checked: bool,
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

    fn unary_op_on(mut self, operator: crate::abs::UnaryOp, operand: Self::Operand) {
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

    fn variant_index(self, variant_index: VariantIndex) {
        self.vars_state
            .mut_place(&self.dest, |_, v| match ValueRef::make_mut(v) {
                Value::Concrete(ConcreteValue::Adt(AdtValue {
                    kind: AdtKind::Enum { discriminant },
                    ..
                })) => *discriminant = variant_index,
                _ => {
                    unreachable!("Assigning variant index is only supposed to requested on enums.")
                }
            })
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
    location: BasicBlockIndex,
    discriminant: ValueRef,
    constraint_manager: &'a mut ConstraintManager,
}

impl<'a> BasicBranchingHandler<'a> {
    fn new(
        location: BasicBlockIndex,
        discriminant: ValueRef,
        constraint_manager: &'a mut ConstraintManager,
    ) -> Self {
        Self {
            location,
            discriminant,
            constraint_manager,
        }
    }
}

impl<'a> BranchingHandler for BasicBranchingHandler<'a> {
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
    parent: BasicBranchingHandler<'a>,
}

impl BranchTakingHandler<bool> for BasicBranchTakingHandler<'_> {
    fn take(self, value: bool) {
        /* FIXME: Bad smell! The branching traits structure prevents
         * us from having a simpler and cleaner handler.
         */
        if !self.parent.discriminant.is_symbolic() {
            return;
        }

        let mut constraint = Constraint::Bool(self.parent.discriminant);
        if !value {
            constraint = constraint.not();
        }

        self.parent.constraint_manager.add(constraint);
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
                fn take(self, value: $type) {
                    if !self.parent.discriminant.is_symbolic() {
                        return;
                    }

                    let expr = Expr::Binary {
                        operator: BinaryOp::Eq,
                        first: SymValueRef::new(self.parent.discriminant),
                        second: ValueRef::new(Value::from_const(ConstValue::from(value))),
                        is_flipped: false,
                    };
                    self.parent.constraint_manager.add(Constraint::Bool(ValueRef::new(Value::Symbolic(SymValue::Expression(expr)))));
                }

                fn take_otherwise(self, non_values: &[$type]) {
                    if !self.parent.discriminant.is_symbolic() {
                        return;
                    }

                    for value in non_values {
                        let expr = Expr::Binary {
                            operator: BinaryOp::Ne,
                            first: SymValueRef::new(self.parent.discriminant.clone()),
                            second: ValueRef::new(Value::from_const(ConstValue::from(*value))),
                            is_flipped: false,
                        };
                        self.parent.constraint_manager.add(Constraint::Bool(ValueRef::new(Value::Symbolic(SymValue::Expression(expr)))));
                    }
                }
            }
        )*
    };
}

impl_general_branch_taking_handler!(u128, char, VariantIndex);

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

    fn call(
        self,
        func: Self::Operand,
        args: impl Iterator<Item = Self::Operand>,
        result_dest: Self::Place,
    ) {
        // TODO: Put arguments in the variables state.
        self.call_stack_manager.push(result_dest)
    }

    fn ret(self) {
        let (result_dest, returned_val) = self.call_stack_manager.pop();
        /* FIXME: May require a cleaner approach. */
        if !self.call_stack_manager.is_empty() {
            self.call_stack_manager
                .top()
                .set_place(&result_dest, returned_val)
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
        let local = place.local();
        let value = self
            .locals
            .remove(&local)
            .expect(Self::get_err_message(&local).as_str());

        let mut value = value;

        if !place.has_projection() {
            return value;
        }

        let (last, projs) = place.projections.split_last().unwrap();
        let last = match last {
            Projection::Field(field) => *field,
            _ => panic!("Move can only happen on locals or partially move on struct fields."),
        };

        let field_val = Self::take_field(self.apply_projs_mut(&mut value, projs.iter()), last);
        self.locals.insert(local, value);
        field_val
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
            })) => (&mut fields[field as usize])
                .take()
                .expect((format!("Field should not be moved before. {field}")).as_str()),
            Value::Symbolic(_) => {
                unimplemented!("Partial move on symbolic values is not currently supported.")
            }
            _ => panic!("Only (concrete) structs can be partially moved."),
        }
    }

    fn get_place<'b>(&self, place: &Place) -> &ValueRef {
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
            .expect(Self::get_err_message(&local).as_str());
        self.apply_projs(host, projs)
    }

    fn apply_projs<'a, 'b>(
        &'a self,
        host: &'a ValueRef,
        mut projs: impl Iterator<Item = &'b Projection>,
    ) -> &'_ ValueRef
    where
        'a: 'b,
    {
        let mut current = host;
        while let Some(proj) = projs.next() {
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
                    ConcreteValue::Adt(AdtValue { fields, .. }) => (&fields[*field as usize])
                        .as_ref()
                        .expect((format!("Field should not be moved before. {field}")).as_str()),
                    _ => panic!("{err}"),
                },
                Projection::Index(index) => match concrete {
                    ConcreteValue::Array(ArrayValue { elements }) => {
                        let index = self.copy_place(&index);
                        match index.as_ref() {
                            Value::Concrete(ConcreteValue::Const(ConstValue::Int {
                                bit_rep,
                                size,
                                is_signed,
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
                    offset,
                    min_length,
                    from_end,
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
                Projection::Subslice { from, to, from_end } => todo!(),
                Projection::Downcast(_) => todo!(),
                Projection::OpaqueCast => todo!(),
            };
        }
        current
    }

    fn mut_place(&mut self, place: &Place, mutate: impl FnOnce(&Self, &mut ValueRef)) {
        self.mut_place_iter(place.local(), &place.projections, mutate);
    }

    fn mut_place_iter<'a, 'b>(
        &'a mut self,
        local: Local,
        projs: &'b [Projection],
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
            .expect(Self::get_err_message(&local).as_str());
        let value = self.apply_projs_mut(&mut local_value, projs.iter());
        mutate(self, value);
        self.locals.insert(local, local_value);
    }

    fn apply_projs_mut<'a, 'b>(
        &'a self,
        host: &'a mut ValueRef,
        mut projs: impl Iterator<Item = &'b Projection>,
    ) -> &'_ mut ValueRef
    where
        'a: 'b,
    {
        let mut current = host;
        while let Some(proj) = projs.next() {
            /* NOTE: Alive copies on projectable types does not seem to be possible.
             * For compound types, it is not observed in the MIR. Only immutable ref
             * can be copied, which is not possible to be given to this function.
             * Therefore, we expect that make_mut is not going to clone when there
             * are some projections.
             */
            let original_addr = ValueRef::as_ptr(&current);
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
                    ConcreteValue::Adt(AdtValue { fields, .. }) => (&mut fields[*field as usize])
                        .as_mut()
                        .expect((format!("Field should not be moved before. {field}")).as_str()),
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
                                size,
                                is_signed,
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
                    offset,
                    min_length,
                    from_end,
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
                Projection::Subslice { from, to, from_end } => todo!(),
                Projection::Downcast(_) => todo!(),
                Projection::OpaqueCast => todo!(),
            };
        }
        current
    }

    #[inline]
    fn get_err_message(local: &Local) -> String {
        format!("Uninitialized, moved, or invalid local. {}", local)
    }
}

enum Constraint {
    Bool(ValueRef),
    Not(ValueRef),
}

impl Constraint {
    fn not(self) -> Constraint {
        match self {
            Constraint::Bool(expr) => Constraint::Not(expr),
            Constraint::Not(expr) => Constraint::Bool(expr),
        }
    }
}

struct ConstraintManager {
    constraints: Vec<Constraint>,
}

impl ConstraintManager {
    fn new() -> Self {
        Self {
            constraints: Vec::new(),
        }
    }

    pub fn add(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }
}

struct CallStackManager {
    stack: Vec<CallStackFrame>,
}

struct CallStackFrame {
    vars_state: MutableVariablesState,
    result_dest: Place,
}

impl CallStackManager {
    fn new() -> Self {
        let mut instance = Self { stack: Vec::new() };
        /* TODO: This is a hack to make sure that a call info exists for the
         * entry point. It will be investigated in #68.
         */
        instance.push(Place::new(0));
        instance
    }

    fn push(&mut self, result_dest: Place) {
        self.stack.push(CallStackFrame {
            vars_state: MutableVariablesState::new(),
            result_dest,
        });
    }

    fn pop(&mut self) -> (Place, ValueRef) {
        let frame = self.stack.pop().expect("Call stack is empty.");
        let mut vars_state = frame.vars_state;
        // TODO: Clean up after better management of special local variables.
        (frame.result_dest, vars_state.take_place(&Place::new(0)))
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
        operand::Constant::Str(value) => ConstValue::Str(*value),
        operand::Constant::Func(value) => ConstValue::Func(*value),
    }
}
