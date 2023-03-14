use std::{
    cell::{Cell, RefCell, RefMut},
    collections::HashMap,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::abs::{
    AssignmentHandler, BranchTakingHandler, BranchingHandler, FieldIndex, FunctionHandler, Local,
    RuntimeBackend, VariantIndex,
};

use self::{
    expr::{AdtKind, AdtValue, ConcreteValue, ConstValue, RefValue, Value},
    operand::{DefaultOperandHandler, Operand},
    place::{DefaultPlaceHandler, Place, Projection},
};

pub(crate) mod expr;

pub(crate) mod operand;
pub(crate) mod place;

mod utils;

pub(crate) mod logger;

pub struct BasicBackend;

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

    type BranchingHandler<'a> = BasicBranchingHandler
    where
        Self: 'a;

    type FunctionHandler<'a> = BasicFunctionHandler
    where
        Self: 'a;

    type Place = Place;

    type Operand = Operand;

    fn place<'a>(&'a mut self) -> Self::PlaceHandler<'a> {
        todo!()
    }

    fn operand<'a>(&'a mut self) -> Self::OperandHandler<'a> {
        todo!()
    }

    fn assign_to<'a>(
        &'a mut self,
        dest: <Self::AssignmentHandler<'a> as crate::abs::AssignmentHandler>::Place,
    ) -> Self::AssignmentHandler<'a> {
        todo!()
    }

    fn branch<'a>(
        &'a mut self,
        location: crate::abs::BasicBlockIndex,
        discriminant: <Self::OperandHandler<'static> as crate::abs::OperandHandler>::Operand,
    ) -> Self::BranchingHandler<'a> {
        todo!()
    }

    fn func_control<'a>(&'a mut self) -> Self::FunctionHandler<'a> {
        todo!()
    }
}

pub(crate) struct BasicAssignmentHandler<'s> {
    vars_state: &'s mut MutableVariablesState,
    dest: Place,
}

impl AssignmentHandler for BasicAssignmentHandler<'_> {
    type Place = Place;
    type Operand = Operand;

    fn use_of(self, operand: Self::Operand) {}

    fn repeat_of(self, operand: Self::Operand, count: usize) {}

    fn ref_to(self, place: Self::Place, is_mutable: bool) {
        todo!()
    }

    fn thread_local_ref_to(self) {
        todo!()
    }

    fn address_of(self, place: Self::Place, is_mutable: bool) {
        todo!()
    }

    fn len_of(self, place: Self::Place) {
        todo!()
    }

    fn numeric_cast_of(self, operand: Self::Operand, is_to_float: bool, size: usize) {
        todo!()
    }

    fn cast_of(self) {
        todo!()
    }

    fn binary_op_between(
        self,
        operator: crate::abs::BinaryOp,
        first: Self::Operand,
        second: Self::Operand,
        checked: bool,
    ) {
        todo!()
    }

    fn unary_op_on(self, operator: crate::abs::UnaryOp, operand: Self::Operand) {
        todo!()
    }

    fn discriminant_of(self, place: Self::Place) {
        todo!()
    }

    fn array_from(self, items: impl Iterator<Item = Self::Operand>) {
        todo!()
    }
}

impl BasicAssignmentHandler<'_> {
    fn get_dest_expr(self) -> ValueRef {
        todo!()
        // self.get_place_expr(self.dest)
    }

    fn get_operand_expr(
        &self,
        operand: <BasicAssignmentHandler as AssignmentHandler>::Operand,
    ) -> ValueRef {
        match operand {
            Operand::Place(place, _) => self.get_place_expr(place),
            Operand::Const(constant) => ValueRef::new(Value::Concrete(todo!())),
        }
    }

    fn get_constant_expr(&self, constant: operand::Constant) -> ConstValue {
        match constant {
            operand::Constant::Bool(value) => ConstValue::Bool(value),
            operand::Constant::Char(value) => ConstValue::Char(value),
            operand::Constant::Int {
                bit_rep,
                size,
                is_signed,
            } => ConstValue::Int {
                bit_rep,
                size,
                is_signed,
            },
            operand::Constant::Float {
                bit_rep,
                ebits,
                sbits,
            } => ConstValue::Float {
                bit_rep,
                ebits,
                sbits,
            },
            operand::Constant::Str(value) => ConstValue::Str(value),
            operand::Constant::Func(value) => ConstValue::Func(value),
        }
    }

    fn get_place_expr(&self, place: Place) -> ValueRef {
        todo!()
    }

    fn get_projection_expr(&self, kind: Projection, on: Place) -> Value {
        return todo!();
        let on_expr = self.get_place_expr(on);
        // Value::NonConstant(match kind {
        //     ProjectionKind::Deref => todo!(),
        //     ProjectionKind::Field(field) => Expr::Field {
        //         expr: on_expr,
        //         field,
        //     },
        //     ProjectionKind::Index(index) => Expr::Index {
        //         on: on_expr,
        //         index: self.get_place_expr(*index),
        //         from_end: false,
        //     },
        //     ProjectionKind::ConstantIndex {
        //         offset,
        //         min_length,
        //         from_end,
        //     } => Expr::Index {
        //         // TODO: Check if min length can be used.
        //         on: on_expr,
        //         index: ValueRef::new(Value::Concrete(ConstValue::from(size_of_val(&offset)))),
        //         from_end,
        //     },
        //     ProjectionKind::Subslice { from, to, from_end } => Expr::Slice {
        //         of: on_expr,
        //         from: ValueRef::new(Value::Concrete(ConstValue::from(size_of_val(&from)))),
        //         to: ValueRef::new(Value::Concrete(ConstValue::from(size_of_val(&to)))),
        //         from_end,
        //     },
        //     ProjectionKind::Downcast(_) => todo!(),
        //     ProjectionKind::OpaqueCast => todo!(),
        // })
    }
}

pub(crate) struct BasicBranchingHandler;

impl BranchingHandler for BasicBranchingHandler {
    type BoolBranchTakingHandler = BasicBranchTakingHandler;

    type IntBranchTakingHandler = BasicBranchTakingHandler;

    type CharBranchTakingHandler = BasicBranchTakingHandler;

    type EnumBranchTakingHandler = BasicBranchTakingHandler;

    fn on_bool(self) -> Self::BoolBranchTakingHandler {
        todo!()
    }

    fn on_int(self) -> Self::IntBranchTakingHandler {
        todo!()
    }

    fn on_char(self) -> Self::CharBranchTakingHandler {
        todo!()
    }

    fn on_enum(self) -> Self::EnumBranchTakingHandler {
        todo!()
    }
}

pub(crate) struct BasicBranchTakingHandler;

impl BranchTakingHandler<bool> for BasicBranchTakingHandler {
    fn take(self, value: bool) {
        todo!()
    }

    fn take_otherwise(self, non_values: &[bool]) {
        todo!()
    }
}

impl BranchTakingHandler<u128> for BasicBranchTakingHandler {
    fn take(self, value: u128) {
        todo!()
    }

    fn take_otherwise(self, non_values: &[u128]) {
        todo!()
    }
}

impl BranchTakingHandler<char> for BasicBranchTakingHandler {
    fn take(self, value: char) {
        todo!()
    }

    fn take_otherwise(self, non_values: &[char]) {
        todo!()
    }
}

impl BranchTakingHandler<VariantIndex> for BasicBranchTakingHandler {
    fn take(self, value: VariantIndex) {
        todo!()
    }

    fn take_otherwise(self, non_values: &[VariantIndex]) {
        todo!()
    }
}

pub(crate) struct BasicFunctionHandler;

impl FunctionHandler for BasicFunctionHandler {
    type Place = Place;
    type Operand = Operand;

    fn call(
        self,
        func: Self::Operand,
        args: impl Iterator<Item = Self::Operand>,
        result_dest: Self::Place,
    ) {
        todo!()
    }

    fn ret(self) {
        todo!()
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

    pub fn set_place(&mut self, place: &Place, value: Value) {
        let local = place.local();
        if !place.has_projection() {
            self.locals.insert(local, ValueRef::new(value));
        } else {
            self.mut_place(place, |s, v| *v = ValueRef::new(value));
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
                    ConcreteValue::Array { elements, .. } => {
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
                    ConcreteValue::Array { elements, .. } => {
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
        self.mut_place_iter(place.local(), place.projections.as_slice(), mutate);
    }

    fn mut_place_iter<'a, 'b>(
        &'a mut self,
        local: Local,
        projs: &'b [Projection],
        mutate: impl FnOnce(&Self, &mut ValueRef),
    ) {
        // Up to the last deref, we only need the place. So no mutable borrow is needed.
        if let Some(deref_index) = projs.iter().rposition(|p| matches!(p, Projection::Deref)) {
            let value = self.get_place_iter(local, projs.iter().take(deref_index));
            match value.as_ref() {
                Value::Concrete(ConcreteValue::Ref(RefValue::Mut(place))) => {
                    self.mut_place(&place.clone(), |s, v| {
                        mutate(s, s.apply_projs_mut(v, projs[deref_index + 1..].iter()));
                    });
                    return;
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
                    ConcreteValue::Array { elements, .. } => {
                        let index = self.copy_place(&index);
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
                    ConcreteValue::Array { elements, .. } => {
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

    #[inline]
    fn get_mut(value_ref: &mut ValueRef, value_info: String) -> &mut Value {
        ValueRef::get_mut(value_ref)
            .expect(format!("Value ({}) should not be in use.", value_info).as_str())
    }
    #[inline]
    fn peel_off(value_ref: ValueRef, value_info: String) -> Value {
        ValueRef::try_unwrap(value_ref)
            .ok()
            .expect(format!("Value ({}) should not be in use.", value_info).as_str())
    }
}

struct ConstraintManager {
    constraints: Vec<Constraint>,
}

enum Constraint {
    Bool(expr::ValueRef),
    Not(expr::ValueRef),
}

impl Constraint {
    fn not(self) -> Constraint {
        match self {
            Constraint::Bool(expr) => Constraint::Not(expr),
            Constraint::Not(expr) => Constraint::Bool(expr),
        }
    }
}

struct ValueMutRef<'a> {
    value: RefMut<'a, ValueRef>,
    host: Option<Rc<RefCell<ValueRef>>>,
    put_back: Box<dyn FnOnce(ValueRef) + 'a>,
}

impl<'a> Deref for ValueMutRef<'a> {
    type Target = ValueRef;

    fn deref(&self) -> &Self::Target {
        &self.value.deref()
    }
}

impl<'a> DerefMut for ValueMutRef<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.value.deref_mut()
    }
}

impl Drop for ValueMutRef<'_> {
    fn drop(&mut self) {

        // (self.put_back)(self.host.into_inner())
    }
}
