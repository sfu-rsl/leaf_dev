use crate::expression::Expression::Symbolic;
use crate::{Context, Solver};
use leafcommon::consts::Const;
use leafcommon::misc::{DebugInfo, PlaceAndDebugInfo};
use leafcommon::place::{Local, Place};
use leafcommon::rvalue::{BinOp, Constant, ConstantKind, Operand, OperandVec, Rvalue};
use leafcommon::switchtargets::SwitchTargets;
use leafcommon::ty::IntTy::I32;
use leafcommon::ty::{FloatTy, IntTy, TyKind};
use paste::paste;
use std::collections::{HashMap, HashSet};
use std::env::var;
use std::fmt::{Debug, Formatter};
use std::ops::Rem;
use std::rc::Rc;
use std::str::FromStr;
use z3::ast::{Ast, Bool, Int};
use z3::SatResult;

/// Contains an insertion result from an [Operand] insert. Since an Operand insert can use Move
#[derive(Debug, Eq, PartialEq)]
enum InsertResult {
    Noop(bool),
    Inserted(bool),
    NeedsRemoval(bool, Local),
}

impl InsertResult {
    fn is_inserted(&self) -> bool {
        if let InsertResult::Noop(_) = self {
            true
        } else {
            false
        }
    }

    fn is_symbolic(&self) -> bool {
        match self {
            InsertResult::Noop(s) => *s,
            InsertResult::Inserted(s) => *s,
            InsertResult::NeedsRemoval(s, _) => *s,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum SymbolicType {
    ExplicitSymbolic,
    InvolvesSymbolic,
    Concrete,
}

impl SymbolicType {
    fn is_symbolic(&self) -> bool {
        match self {
            SymbolicType::ExplicitSymbolic | SymbolicType::InvolvesSymbolic => true,
            SymbolicType::Concrete => false,
        }
    }
}

fn create_ast_based_on_ty(
    ctx: &Context,
    ty: TyKind,
    variable_name: String,
    serialized_const_value: Option<String>,
) -> AstType {
    match ty {
        TyKind::Bool => AstType::Bool(
            if let Some(serialized_const_value) = serialized_const_value {
                z3::ast::Bool::from_bool(&ctx.0, bool::from_str(&serialized_const_value).unwrap())
            } else {
                z3::ast::Bool::new_const(&ctx.0, variable_name.clone())
            },
        ),
        TyKind::Char => todo!(),
        TyKind::Int(_) | TyKind::Uint(_) => AstType::Int(
            if let Some(serialized_const_value) = serialized_const_value {
                z3::ast::Int::from_str(&ctx.0, &serialized_const_value).unwrap()
            } else {
                z3::ast::Int::new_const(&ctx.0, variable_name.clone())
            },
        ),
        TyKind::Float(FloatTy::F32) => AstType::Float(
            if let Some(serialized_const_value) = serialized_const_value {
                z3::ast::Float::from_f32(&ctx.0, f32::from_str(&serialized_const_value).unwrap())
            } else {
                z3::ast::Float::new_const_float32(&ctx.0, variable_name.clone())
            },
        ),
        TyKind::Float(FloatTy::F64) => AstType::Float(
            if let Some(serialized_const_value) = serialized_const_value {
                z3::ast::Float::from_f64(&ctx.0, f64::from_str(&serialized_const_value).unwrap())
            } else {
                z3::ast::Float::new_const_double(&ctx.0, variable_name.clone())
            },
        ),
        TyKind::Adt => todo!(),
        TyKind::Foreign => todo!(),
        TyKind::Str => AstType::String(
            if let Some(serialized_const_value) = serialized_const_value {
                z3::ast::String::from_str(&ctx.0, &serialized_const_value).unwrap()
            } else {
                z3::ast::String::new_const(&ctx.0, variable_name.clone())
            },
        ),
        TyKind::Array => todo!(),
        TyKind::Slice => todo!(),
        TyKind::RawPtr(_) => todo!(),
        TyKind::Ref(_) => todo!(),
        TyKind::FnDef => todo!(),
        TyKind::FnPtr => todo!(),
        TyKind::Dynamic => todo!(),
        TyKind::Closure => todo!(),
        TyKind::Generator => todo!(),
        TyKind::GeneratorWitness => todo!(),
        TyKind::Never => todo!(),
        TyKind::Tuple => todo!(),
        TyKind::Projection => todo!(),
        TyKind::Opaque => todo!(),
        TyKind::Param => todo!(),
        TyKind::Bound => todo!(),
        TyKind::Placeholder => todo!(),
        TyKind::Infer => todo!(),
        TyKind::Error => todo!(),
    }
}

fn generate_variable_name(
    fn_name: Option<&String>,
    destination: &Place,
    debug_info: Option<&DebugInfo>,
) -> String {
    if let Some(DebugInfo { name: Some(name) }) = debug_info {
        String::from(name)
    } else {
        let mut name = String::new();
        if let Some(fn_name) = fn_name {
            name.push_str(fn_name);
        }
        name.push('_');
        name.push_str(&*destination.local.private().to_string());
        name
    }
}

#[derive(Debug, Default)]
pub struct PlaceMap<'ctx> {
    pub(crate) map: HashMap<Local, Expression<'ctx>>,
}

fn place_from_operand(operand: &Operand) -> Option<&Place> {
    match operand {
        Operand::Copy(ref place) => Some(place),
        Operand::Move(ref place) => Some(place),
        Operand::Constant(_) => None,
    }
}

impl<'ctx> PlaceMap<'ctx> {
    fn new() -> Self {
        Self {
            map: HashMap::default(),
        }
    }

    fn operand_symbolic_type(
        &self,
        operand: &Operand,
        debug_info: Option<&DebugInfo>,
    ) -> SymbolicType {
        let place = place_from_operand(operand);
        // See if the Rvalue refers to an existing place
        let mut symbolic_type = place.map_or(SymbolicType::Concrete, |place| {
            self.map
                .get(&place.local)
                .map_or(SymbolicType::Concrete, |expr| expr.symbolic_type())
        });

        if !symbolic_type.is_symbolic() {
            if let Some(DebugInfo { name: Some(name) }) = debug_info {
                if name.contains("leaf_symbolic") {
                    return SymbolicType::ExplicitSymbolic;
                }
            }
        };

        return symbolic_type;
    }

    pub fn create_switch_int_formula(
        &self,
        solver: Solver,
        place: &Place,
        switch_targets: SwitchTargets,
    ) -> Result<z3::ast::Int, ()> {
        let expression = self.map.get(&place.local).ok_or(())?;
        match expression {
            Expression::Symbolic { .. } => {}
            Expression::ConcreteConstant { .. } => {}
            Expression::Rvalue { .. } => {}
        }
        Err(())
    }

    pub fn insert_expr(&mut self, destination: &Place, expr: Expression<'ctx>) {
        self.map.insert(destination.local, expr);
    }

    pub fn expr_from_operand<'a>(&self, operand: &'a Operand) -> &Expression<'ctx> {
        let place = place_from_operand(operand).unwrap();
        self.map
            .get(&place.local)
            .expect("expression should be present")
    }

    fn handle_binary_op_ast_creation(
        &self,
        ctx: &'ctx Context,
        bin_op: &BinOp,
        operand_pair: &Box<(Operand, Operand)>,
    ) -> (SymbolicType, AstType<'ctx>, TyKind) {
        let (left, right) = &**operand_pair;
        let left = self.expr_from_operand(left);
        let right = self.expr_from_operand(right);

        let symbolic_type = {
            if left.symbolic_type().is_symbolic() || right.symbolic_type().is_symbolic() {
                SymbolicType::InvolvesSymbolic
            } else {
                SymbolicType::Concrete
            }
        };

        let (ast_type, ty_kind) = match bin_op {
            BinOp::Add => match left.ast_type() {
                AstType::Bool(_) | AstType::String(_) => unreachable!(),
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        (
                            AstType::Int(z3::ast::Int::add(&ctx.0, &[left_ast, right_ast])),
                            left.ty_kind().clone(),
                        )
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float(left_ast) => {
                    if let AstType::Float(right_ast) = right.ast_type() {
                        (
                            AstType::Float(left_ast.add_towards_zero(right_ast)),
                            left.ty_kind().clone(),
                        )
                    } else {
                        unreachable!()
                    }
                }
            },
            BinOp::Sub => match left.ast_type() {
                AstType::Bool(_) | AstType::String(_) => unreachable!(),
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        (
                            AstType::Int(z3::ast::Int::sub(&ctx.0, &[left_ast, right_ast])),
                            left.ty_kind().clone(),
                        )
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float(left_ast) => {
                    if let AstType::Float(right_ast) = right.ast_type() {
                        (AstType::Float(left_ast.sub_towards_zero(right_ast)), left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
            },
            BinOp::Mul => match left.ast_type() {
                AstType::Bool(_) | AstType::String(_) => unreachable!(),
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        (AstType::Int(z3::ast::Int::mul(&ctx.0, &[left_ast, right_ast])), left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float(left_ast) => {
                    if let AstType::Float(right_ast) = right.ast_type() {
                        (AstType::Float(left_ast.mul_towards_zero(right_ast)), left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
            },
            BinOp::Div => match left.ast_type() {
                AstType::Bool(_) | AstType::String(_) => unreachable!(),
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        (AstType::Int(left_ast.div(right_ast)), left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float(left_ast) => {
                    if let AstType::Float(right_ast) = right.ast_type() {
                        (AstType::Float(left_ast.div_towards_zero(right_ast)), left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
            },
            BinOp::Rem => todo!(),
            BinOp::BitXor => todo!(),
            BinOp::BitAnd => todo!(),
            BinOp::BitOr => todo!(),
            BinOp::Shl => todo!(),
            BinOp::Shr => todo!(),
            // Note: Eq, Lt, Le, Ne, Ge, Gt will return 0 or 1, but we have to interpret it as a
            // boolean, since we don't know how to cast booleans into integers with the Z3 crate.
            // Z3 supposedly supports casting / type coercion though.
            BinOp::Eq => todo!(),
            BinOp::Lt => todo!(),
            BinOp::Le => todo!(),
            BinOp::Ne => todo!(),
            BinOp::Ge => match left.ast_type() {
                AstType::Bool(_) | AstType::String(_) => unreachable!(),
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        (AstType::Bool(left_ast.ge(right_ast)), TyKind::Bool)
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float(left_ast) => {
                    if let AstType::Float(right_ast) = right.ast_type() {
                        (AstType::Bool(left_ast.ge(right_ast)), TyKind::Bool)
                    } else {
                        unreachable!("{:?}", right)
                    }
                }
            },
            BinOp::Gt => match left.ast_type() {
                AstType::Bool(_) | AstType::String(_) => unreachable!(),
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        (AstType::Bool(left_ast.gt(right_ast)), TyKind::Bool)
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float(left_ast) => {
                    if let AstType::Float(right_ast) = right.ast_type() {
                        (AstType::Bool(left_ast.gt(right_ast)), TyKind::Bool)
                    } else {
                        unreachable!("{:?}", right)
                    }
                }
            },
            BinOp::Offset => todo!(),
        };
        (symbolic_type, ast_type, ty_kind)
    }

    pub fn insert_rvalue(
        &mut self,
        ctx: &'ctx Context,
        fn_name: Option<&String>,
        destination: &Place,
        rvalue: Rvalue,
        ty: Option<TyKind>,
        debug_info: Option<&DebugInfo>,
        serialized_constant_value: Option<String>,
    ) {
        // dbg!(&rvalue);
        let variable_name = generate_variable_name(fn_name, destination, debug_info);

        let (symbolic_type, ast_type, ty) = match &rvalue {
            Rvalue::Use(operand) => {
                let ty = if let Some(ty) = ty {
                    ty
                } else {
                    let expr = self.expr_from_operand(operand);
                    expr.ty_kind().clone()
                };

                (
                    self.operand_symbolic_type(operand, debug_info),
                    create_ast_based_on_ty(
                        ctx,
                        ty.clone(),
                        variable_name.clone(),
                        serialized_constant_value,
                    ),
                    ty,
                )
            }
            Rvalue::Repeat(_, _) => todo!(),
            Rvalue::Ref(_, _) => todo!(),
            Rvalue::ThreadLocalRef => todo!(),
            Rvalue::AddressOf(_, _) => todo!(),
            Rvalue::Len(_) => todo!(),
            Rvalue::Cast(_, _, _) => todo!(),
            Rvalue::BinaryOp(b, operand_pair) => {
                self.handle_binary_op_ast_creation(ctx, b, operand_pair)
            }
            Rvalue::CheckedBinaryOp(b, operand_pair) => {
                // TODO: Detect overflows?
                self.handle_binary_op_ast_creation(ctx, b, operand_pair)
            },
            Rvalue::NullaryOp(_, _) => todo!(),
            Rvalue::UnaryOp(_, _) => todo!(),
            Rvalue::Discriminant(_) => todo!(),
            Rvalue::Aggregate(_, _) => todo!(),
            Rvalue::ShallowInitBox(_, _) => todo!(),
        };
        match symbolic_type {
            SymbolicType::ExplicitSymbolic => {
                self.map.insert(
                    destination.local,
                    Expression::Symbolic {
                        place: destination.local,
                        ty,
                        ast_type,
                        variable_name,
                    },
                );
            }
            SymbolicType::InvolvesSymbolic | SymbolicType::Concrete => {
                self.map.insert(
                    destination.local,
                    Expression::Rvalue {
                        ty,
                        ast_type,
                        is_symbolic: symbolic_type.is_symbolic(),
                    },
                );
            }
        };
    }

    fn get(&'ctx self, place: &Place) -> Option<&'ctx Expression<'ctx>> {
        self.map.get(&place.local)
    }

    fn contains(&self, place: &Place) -> bool {
        self.map.contains_key(&place.local)
    }

    fn remove(&mut self, place: &Place) -> Option<Expression> {
        self.map.remove(&place.local)
    }
}

#[derive(Debug)]
pub enum FunctionCallContext<'ctx> {
    NoReturn {
        function_name: Option<String>,
        level: usize,
        place_map: PlaceMap<'ctx>,
    },
    WithReturn {
        function_name: Option<String>,
        level: usize,
        place_map: PlaceMap<'ctx>,
        last_seen_type: Option<TyKind>,
        last_const_val: Option<String>,
        destination: PlaceAndDebugInfo,
    },
}

impl<'ctx> FunctionCallContext<'ctx> {
    pub fn function_name(&self) -> Option<&String> {
        match self {
            FunctionCallContext::NoReturn { function_name, .. } => function_name.as_ref(),
            FunctionCallContext::WithReturn { function_name, .. } => function_name.as_ref(),
        }
    }

    pub fn level(&self) -> usize {
        match self {
            FunctionCallContext::NoReturn { level, .. } => *level,
            FunctionCallContext::WithReturn { level, .. } => *level,
        }
    }

    pub fn place_map(&self) -> &PlaceMap<'ctx> {
        match self {
            FunctionCallContext::NoReturn { place_map, .. } => place_map,
            FunctionCallContext::WithReturn { place_map, .. } => place_map,
        }
    }

    pub fn place_map_mut(&mut self) -> &mut PlaceMap<'ctx> {
        match self {
            FunctionCallContext::NoReturn { place_map, .. } => place_map,
            FunctionCallContext::WithReturn { place_map, .. } => place_map,
        }
    }
}

#[derive(Debug)]
pub struct FunctionCallStack<'ctx> {
    stack: Vec<FunctionCallContext<'ctx>>,
}

impl<'ctx> FunctionCallStack<'ctx> {
    pub fn new() -> Self {
        // TODO: Detect whether the main function has a return value or not
        Self {
            stack: vec![FunctionCallContext::NoReturn {
                function_name: Some(String::from("main")),
                level: 0,
                place_map: PlaceMap::new(),
            }],
        }
    }

    pub fn handle_switch_int(
        &mut self,
        solver: &Solver,
        discriminant: Operand,
        switch_targets: SwitchTargets,
    ) {
        let solver = &solver.0;
        let place_map = self.current_place_map().unwrap();
        let discriminant = place_map.expr_from_operand(&discriminant);
        let ast_expr = discriminant.ast_type();
        match ast_expr {
            AstType::Bool(discriminant_bool) => {
                let value_targets = switch_targets
                    .switch_targets
                    .iter()
                    .map(|(value, _target)| *value == 1)
                    .collect::<Vec<bool>>();

                for target in value_targets {
                    /*
                    TODO: Preserve the original variable traces.

                    Example program:

                        fn get_int() -> i32 { 6 }

                        fn main() {
                            let leaf_symbolic = get_int();

                            if leaf_symbolic > 5 {
                                "Greater than 5"
                            } else {
                                "Less than 5"
                            };
                        }

                    The MIR looks like this:

                        fn get_int() -> i32 {
                            let mut _0: i32;                     // return place in scope 0 at src/main.rs:1:17: 1:20

                            bb0: {
                                _0 = const 6_i32;                // scope 0 at src/main.rs:2:5: 2:6
                                return;                          // scope 0 at src/main.rs:3:2: 3:2
                            }
                        }

                        fn main() -> () {
                            let mut _0: ();                      // return place in scope 0 at src/main.rs:5:11: 5:11
                            let _1: i32;                         // in scope 0 at src/main.rs:8:9: 8:22
                            let mut _2: bool;                    // in scope 0 at src/main.rs:10:8: 10:25
                            let mut _3: i32;                     // in scope 0 at src/main.rs:10:8: 10:21
                            scope 1 {
                                debug leaf_symbolic => _1;       // in scope 1 at src/main.rs:8:9: 8:22
                            }

                            bb0: {
                                _1 = get_int() -> bb1;           // scope 0 at src/main.rs:8:25: 8:34
                                                                 // mir::Constant
                                                                 // + span: src/main.rs:8:25: 8:32
                                                                 // + literal: Const { ty: fn() -> i32 {get_int}, val: Value(Scalar(<ZST>)) }
                            }

                            bb1: {
                                _3 = _1;                         // scope 1 at src/main.rs:10:8: 10:21
                                _2 = Gt(move _3, const 5_i32);   // scope 1 at src/main.rs:10:8: 10:25
                                switchInt(move _2) -> [false: bb3, otherwise: bb2]; // scope 1 at src/main.rs:10:8: 10:25
                            }

                            bb2: {
                                goto -> bb4;                     // scope 1 at src/main.rs:10:5: 14:6
                            }

                            bb3: {
                                goto -> bb4;                     // scope 1 at src/main.rs:10:5: 14:6
                            }

                            bb4: {
                                return;                          // scope 0 at src/main.rs:15:2: 15:2
                            }
                        }

                    Currently, the solver is only showing that we can set _3 to be 5 in order to go
                    to bb3. Want to show that we can choose _1 as 5.
                     */

                    solver.assert(
                        &discriminant_bool._eq(&Bool::from_bool(solver.get_context(), target)),
                    );

                    match solver.check() {
                        SatResult::Sat => {
                            if target {
                                println!("Reaching the first branch is possible");
                            } else {
                                println!("Reaching the second branch is possible");
                            }

                            // Note: Must check satisfiability before you can get the model.
                            if let Some(model) = solver.get_model() {
                                println!("model: {:?}", model);
                            } else {
                                println!("Failed to get model!");
                            }
                        }
                        SatResult::Unsat => {
                            println!("UNSAT");
                        }
                        _ => {
                            println!("UNKNOWN");
                        }
                    }

                    solver.reset()
                }
            }
            AstType::Int(discriminant_int) => {
                let value_targets = switch_targets
                    .switch_targets
                    .iter()
                    .map(|(value, _target)| *value)
                    .collect::<Vec<u128>>();
            }
            AstType::Float(_) => unreachable!(),
            AstType::String(_) => unreachable!(),
        }
    }

    pub fn handle_ret(&mut self, ctx: &'ctx Context) {
        let mut context_of_returned_function = self.pop();
        if let Some(ref mut context_of_returned_function) = context_of_returned_function {
            if let FunctionCallContext::WithReturn {
                place_map,
                last_seen_type,
                destination:
                    PlaceAndDebugInfo {
                        place: Some(destination),
                        debug_info,
                    },
                ..
            } = context_of_returned_function
            {
                let fn_name = self
                    .current_ctx()
                    .and_then(|ctx| ctx.function_name().cloned());
                let map_of_parent_caller = self.current_place_map_mut().unwrap();
                if let Some(DebugInfo { name: Some(name) }) = debug_info {
                    if name.contains("leaf_symbolic") {
                        let variable_name = generate_variable_name(
                            fn_name.as_ref(),
                            destination,
                            debug_info.as_ref(),
                        );

                        let ty = last_seen_type.as_ref().unwrap();

                        let ast_type = match ty {
                            TyKind::Bool => AstType::Bool(z3::ast::Bool::new_const(
                                &ctx.0,
                                variable_name.clone(),
                            )),
                            TyKind::Char => todo!(),
                            TyKind::Int(_) | TyKind::Uint(_) => {
                                AstType::Int(z3::ast::Int::new_const(&ctx.0, variable_name.clone()))
                            }
                            TyKind::Float(FloatTy::F32) => AstType::Float(
                                z3::ast::Float::new_const_float32(&ctx.0, variable_name.clone()),
                            ),
                            TyKind::Float(FloatTy::F64) => AstType::Float(
                                z3::ast::Float::new_const_double(&ctx.0, variable_name.clone()),
                            ),
                            TyKind::Adt => todo!(),
                            TyKind::Foreign => todo!(),
                            TyKind::Str => AstType::String(z3::ast::String::new_const(
                                &ctx.0,
                                variable_name.clone(),
                            )),
                            TyKind::Array => todo!(),
                            TyKind::Slice => todo!(),
                            TyKind::RawPtr(_) => todo!(),
                            TyKind::Ref(_) => todo!(),
                            TyKind::FnDef => todo!(),
                            TyKind::FnPtr => todo!(),
                            TyKind::Dynamic => todo!(),
                            TyKind::Closure => todo!(),
                            TyKind::Generator => todo!(),
                            TyKind::GeneratorWitness => todo!(),
                            TyKind::Never => todo!(),
                            TyKind::Tuple => todo!(),
                            TyKind::Projection => todo!(),
                            TyKind::Opaque => todo!(),
                            TyKind::Param => todo!(),
                            TyKind::Bound => todo!(),
                            TyKind::Placeholder => todo!(),
                            TyKind::Infer => todo!(),
                            TyKind::Error => todo!(),
                        };

                        map_of_parent_caller.insert_expr(
                            &destination,
                            Expression::Symbolic {
                                place: destination.local,
                                ty: ty.clone(),
                                ast_type,
                                variable_name,
                            },
                        );
                    }
                } else {
                    static LOCAL_PLACE: Local = Local { private: 0 };
                    let expr_from_returned_function = place_map.map.remove(&LOCAL_PLACE).unwrap();
                    map_of_parent_caller.insert_expr(&destination, expr_from_returned_function);
                }
            }
        }
        println!(
            "[ret] latest_ctx = {:?}, FUNCTION_CALL_STACK = {:?}",
            context_of_returned_function, self,
        );
    }

    pub fn handle_assign(
        &mut self,
        context: &'ctx Context,
        place_and_debug_info: &PlaceAndDebugInfo,
        rvalue: Option<&Rvalue>,
        ty: Option<TyKind>,
        serialized_const_value: Option<String>,
    ) {
        // If the local variable is _0, then it's the return value.
        if let Some(FunctionCallContext::WithReturn {
            last_seen_type,
            last_const_val,
            ..
        }) = self.current_ctx_mut()
        {
            *last_seen_type = ty.clone();
            if let Some(ref const_val) = serialized_const_value {
                *last_const_val = Some(const_val.clone());
            }
        }

        let fn_name = self
            .current_ctx()
            .and_then(|ctx| ctx.function_name().cloned());
        let place_map = self.current_place_map_mut().unwrap();

        if let Some(place) = &place_and_debug_info.place {
            if let Some(rvalue) = rvalue {
                place_map.insert_rvalue(
                    context,
                    fn_name.as_ref(),
                    place,
                    rvalue.clone(),
                    ty,
                    place_and_debug_info.debug_info.as_ref(),
                    serialized_const_value,
                );
            }
        }
    }

    pub fn push(
        &mut self,
        function_debug_info: DebugInfo,
        return_destination: PlaceAndDebugInfo,
        args: OperandVec,
    ) {
        let place_map = PlaceMap::new();
        for arg in args.0 {
            match arg {
                Operand::Copy(p) | Operand::Move(p) => {
                    // TODO: For function calls arguments, get the Rvalue
                }
                _ => {}
            }
        }

        let level = self.stack.len();
        if return_destination.place.is_some() {
            self.stack.push(FunctionCallContext::WithReturn {
                function_name: function_debug_info.name,
                level,
                place_map,
                last_seen_type: None,
                last_const_val: None,
                destination: return_destination,
            })
        } else {
            self.stack.push(FunctionCallContext::NoReturn {
                function_name: function_debug_info.name,
                level,
                place_map,
            })
        }
    }

    pub fn pop(&mut self) -> Option<FunctionCallContext<'ctx>> {
        self.stack.pop()
    }

    pub fn current_ctx(&self) -> Option<&FunctionCallContext<'ctx>> {
        self.stack.last()
    }

    pub fn current_ctx_mut(&mut self) -> Option<&mut FunctionCallContext<'ctx>> {
        self.stack.last_mut()
    }

    pub fn current_place_map(&self) -> Option<&PlaceMap<'ctx>> {
        self.stack.last().map(|ctx| ctx.place_map())
    }

    pub fn current_place_map_mut(&mut self) -> Option<&mut PlaceMap<'ctx>> {
        self.stack.last_mut().map(|ctx| ctx.place_map_mut())
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct SerializedValue(ConstantKind, Option<String>);

macro_rules! build_serialized_value_ty_fns {
    ($t:ty) => {
        paste! {
            pub fn [< from_ $t >](kind: ConstantKind, input: $t) -> Self {
                SerializedValue(kind, Some(serde_json::to_string(&input).unwrap()))
            }

            pub fn [< as_ $t >](&self) -> Option<$t> {
                self.1.as_ref().and_then(|s| serde_json::from_str(&s).ok())
            }
        }
    };
}

impl SerializedValue {
    build_serialized_value_ty_fns!(bool);
    build_serialized_value_ty_fns!(i8);
    build_serialized_value_ty_fns!(i16);
    build_serialized_value_ty_fns!(i32);
    build_serialized_value_ty_fns!(i64);
    build_serialized_value_ty_fns!(i128);
    build_serialized_value_ty_fns!(isize);
}
#[derive(Debug)]
enum AstType<'ctx> {
    Bool(z3::ast::Bool<'ctx>),
    Int(z3::ast::Int<'ctx>),
    Float(z3::ast::Float<'ctx>),
    String(z3::ast::String<'ctx>),
}

unsafe impl<'a> std::marker::Sync for AstType<'a> {}

unsafe impl<'a> std::marker::Send for AstType<'a> {}

#[derive(Debug)]
pub enum Expression<'ctx> {
    Symbolic {
        place: Local,
        ty: TyKind,
        ast_type: AstType<'ctx>,
        variable_name: String,
    },
    ConcreteConstant {
        place: Local,
        ty: TyKind,
        ast_type: AstType<'ctx>,
        serialized_value: SerializedValue,
    },
    Rvalue {
        ast_type: AstType<'ctx>,
        ty: TyKind,
        /// Whether any of the nodes inside of [ast_type] is symbolic.
        is_symbolic: bool,
    },
}

impl<'ctx> Expression<'ctx> {
    fn ty_kind(&self) -> &TyKind {
        match self {
            Expression::Symbolic { ty, .. } => ty,
            Expression::ConcreteConstant { ty, .. } => ty,
            Expression::Rvalue { ty, .. } => ty,
        }
    }

    fn ast_type(&self) -> &AstType<'ctx> {
        match self {
            Expression::Symbolic { ast_type, .. } => ast_type,
            Expression::ConcreteConstant { ast_type, .. } => ast_type,
            Expression::Rvalue { ast_type, .. } => ast_type,
        }
    }

    fn symbolic_type(&self) -> SymbolicType {
        match self {
            Expression::Symbolic { .. } => SymbolicType::ExplicitSymbolic,
            Expression::ConcreteConstant { .. } => SymbolicType::Concrete,
            Expression::Rvalue { is_symbolic, .. } => {
                if *is_symbolic {
                    SymbolicType::InvolvesSymbolic
                } else {
                    SymbolicType::Concrete
                }
            }
        }
    }
}

mod test {
    use crate::expression::SerializedValue;
    use crate::ConstantKind::Ty;
    use leafcommon::consts::ConstValue::Scalar;
    use leafcommon::consts::Scalar::Int;
    use leafcommon::rvalue::ConstantKind;
    use leafcommon::ty::IntTy::I32;
    use leafcommon::ty::TyKind;

    #[test]
    fn test_serialized_value_macros() {
        let serialized = SerializedValue::from_i32(
            ConstantKind::Val(Scalar(Int), leafcommon::ty::Ty::new(TyKind::Int(I32))),
            5,
        );
        assert_eq!(5, serialized.as_i32().unwrap());
    }
}
