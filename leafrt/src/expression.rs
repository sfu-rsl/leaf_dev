use crate::expression::Expression::Symbolic;
use crate::{Context, Solver};
use leafcommon::consts::Const;
use leafcommon::misc::{DebugInfo, PlaceAndDebugInfo};
use leafcommon::place::{Local, Place};
use leafcommon::rvalue::{
    BinOp, Constant, ConstantKind, Operand, OperandConstValueVec, OperandVec, Rvalue,
};
use leafcommon::switchtargets::SwitchTargets;
use leafcommon::ty::IntTy::I32;
use leafcommon::ty::{FloatTy, IntTy, Ty, TyKind};
use paste::paste;
use std::collections::{HashMap, HashSet};
use std::env::var;
use std::fmt::{Debug, Formatter};
use std::ops::{Deref, Rem};
use std::rc::Rc;
use std::slice::Iter;
use std::str::FromStr;
use std::sync::Arc;
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

#[derive(Debug, Clone)]
enum SymbolicType<'ctx> {
    ExplicitSymbolic,
    InvolvesSymbolic {
        dependencies: Vec<Arc<Expression<'ctx>>>,
    },
    Concrete {
        dependencies: Vec<Arc<Expression<'ctx>>>,
    },
}

impl<'ctx> SymbolicType<'ctx> {
    fn is_symbolic(&self) -> bool {
        match self {
            SymbolicType::ExplicitSymbolic | SymbolicType::InvolvesSymbolic { .. } => true,
            SymbolicType::Concrete { .. } => false,
        }
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

/// Maps MIR `Place`s to Z3 expressions.
#[derive(Debug, Default)]
pub struct PlaceMap<'ctx> {
    pub(crate) map: HashMap<Local, Arc<Expression<'ctx>>>,
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
    ) -> SymbolicType<'ctx> {
        // See if the Rvalue refers to an existing place
        let symbolic_type = place_from_operand(operand)
            .and_then(|place| self.map.get(&place.local))
            .map(|expr| match expr.deref() {
                Expression::Symbolic { .. } => SymbolicType::InvolvesSymbolic {
                    dependencies: vec![Arc::clone(expr)],
                },
                Expression::Rvalue { is_symbolic, .. } if *is_symbolic => {
                    SymbolicType::InvolvesSymbolic {
                        dependencies: vec![Arc::clone(expr)],
                    }
                }
                _ => SymbolicType::Concrete {
                    dependencies: vec![Arc::clone(expr)],
                },
            })
            .unwrap_or_else(|| SymbolicType::Concrete {
                dependencies: vec![],
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

    pub fn insert_expr_from_local(&mut self, destination: Local, expr: Arc<Expression<'ctx>>) {
        self.map.insert(destination, expr);
    }

    pub fn insert_expr(&mut self, destination: &Place, expr: Arc<Expression<'ctx>>) {
        self.map.insert(destination.local, expr);
    }

    pub fn expr_from_operand<'a>(&self, operand: &'a Operand) -> Option<Arc<Expression<'ctx>>> {
        let place = place_from_operand(operand)?;
        let expr = self.map.get(&place.local)?;
        Some(Arc::clone(expr))
    }

    fn handle_binary_op_ast_creation(
        &self,
        ctx: &'ctx Context,
        fn_name: Option<&String>,
        destination: &Place,
        debug_info: Option<&DebugInfo>,
        bin_op: &BinOp,
        operand_pair: &Box<(Operand, Operand)>,
    ) -> (SymbolicType<'ctx>, AstTypeAndFormulas<'ctx>, TyKind) {
        let (left, right) = &**operand_pair;
        let left = self.expr_from_operand(left).expect("present");
        let right = self.expr_from_operand(right).expect("present");

        let symbolic_type = {
            let dependencies = vec![Arc::clone(&left), Arc::clone(&right)];
            if left.is_symbolic() || right.is_symbolic() {
                SymbolicType::InvolvesSymbolic { dependencies }
            } else {
                SymbolicType::Concrete { dependencies }
            }
        };

        let variable_name = generate_variable_name(fn_name, destination, debug_info);

        let (ast_type, formulas, ty_kind) = match bin_op {
            BinOp::Add => match left.ast_type() {
                AstType::Bool(_) | AstType::String(_) => unreachable!(),
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        let sum = z3::ast::Int::new_const(&ctx.0, variable_name);
                        let formula = sum._eq(&z3::ast::Int::add(&ctx.0, &[left_ast, right_ast]));
                        (AstType::Int(sum), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float {
                    ast: left_ast,
                    is_f32,
                } => {
                    if let AstType::Float { ast: right_ast, .. } = right.ast_type() {
                        let sum = if *is_f32 {
                            z3::ast::Float::new_const_float32(&ctx.0, variable_name)
                        } else {
                            z3::ast::Float::new_const_double(&ctx.0, variable_name)
                        };
                        let formula = sum._eq(&left_ast.add_towards_zero(right_ast));
                        (
                            AstType::Float {
                                ast: sum,
                                is_f32: *is_f32,
                            },
                            vec![formula],
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
                        let difference = z3::ast::Int::new_const(&ctx.0, variable_name);
                        let formula =
                            difference._eq(&z3::ast::Int::sub(&ctx.0, &[left_ast, right_ast]));
                        (
                            AstType::Int(difference),
                            vec![formula],
                            left.ty_kind().clone(),
                        )
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float {
                    ast: left_ast,
                    is_f32,
                } => {
                    if let AstType::Float { ast: right_ast, .. } = right.ast_type() {
                        let difference = if *is_f32 {
                            z3::ast::Float::new_const_float32(&ctx.0, variable_name)
                        } else {
                            z3::ast::Float::new_const_double(&ctx.0, variable_name)
                        };
                        let formula = difference._eq(&left_ast.sub_towards_zero(right_ast));
                        (
                            AstType::Float {
                                ast: difference,
                                is_f32: *is_f32,
                            },
                            vec![formula],
                            left.ty_kind().clone(),
                        )
                    } else {
                        unreachable!()
                    }
                }
            },
            BinOp::Mul => match left.ast_type() {
                AstType::Bool(_) | AstType::String(_) => unreachable!(),
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        let product = z3::ast::Int::new_const(&ctx.0, variable_name);
                        let formula =
                            product._eq(&z3::ast::Int::mul(&ctx.0, &[left_ast, right_ast]));
                        (AstType::Int(product), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float {
                    ast: left_ast,
                    is_f32,
                } => {
                    if let AstType::Float { ast: right_ast, .. } = right.ast_type() {
                        let product = if *is_f32 {
                            z3::ast::Float::new_const_float32(&ctx.0, variable_name)
                        } else {
                            z3::ast::Float::new_const_double(&ctx.0, variable_name)
                        };
                        let formula = product._eq(&left_ast.mul_towards_zero(right_ast));
                        (
                            AstType::Float {
                                ast: product,
                                is_f32: *is_f32,
                            },
                            vec![formula],
                            left.ty_kind().clone(),
                        )
                    } else {
                        unreachable!()
                    }
                }
            },
            BinOp::Div => match left.ast_type() {
                AstType::Bool(_) | AstType::String(_) => unreachable!(),
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        let quotient = z3::ast::Int::new_const(&ctx.0, variable_name);
                        let formula = quotient._eq(&left_ast.div(right_ast));
                        (
                            AstType::Int(quotient),
                            vec![formula],
                            left.ty_kind().clone(),
                        )
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float {
                    ast: left_ast,
                    is_f32,
                } => {
                    if let AstType::Float { ast: right_ast, .. } = right.ast_type() {
                        let quotient = if *is_f32 {
                            z3::ast::Float::new_const_float32(&ctx.0, variable_name)
                        } else {
                            z3::ast::Float::new_const_double(&ctx.0, variable_name)
                        };
                        let formula = quotient._eq(&left_ast.div_towards_zero(right_ast));
                        (
                            AstType::Float {
                                ast: quotient,
                                is_f32: *is_f32,
                            },
                            vec![formula],
                            left.ty_kind().clone(),
                        )
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
            BinOp::Eq => match left.ast_type() {
                AstType::Bool(left_ast) => {
                    if let AstType::Bool(right_ast) = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast._eq(right_ast));
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::String(left_ast) => {
                    if let AstType::String(right_ast) = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast._eq(right_ast));
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast._eq(right_ast));
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float { ast: left_ast, .. } => {
                    if let AstType::Float { ast: right_ast, .. } = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast._eq(right_ast));
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
            },
            BinOp::Lt => todo!(),
            BinOp::Le => todo!(),
            BinOp::Ne => match left.ast_type() {
                AstType::Bool(left_ast) => {
                    if let AstType::Bool(right_ast) = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast._eq(right_ast)).not();
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::String(left_ast) => {
                    if let AstType::String(right_ast) = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast._eq(right_ast)).not();
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast._eq(right_ast)).not();
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float { ast: left_ast, .. } => {
                    if let AstType::Float { ast: right_ast, .. } = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast._eq(right_ast)).not();
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
            },
            BinOp::Ge => match left.ast_type() {
                AstType::Bool(_) | AstType::String(_) => unreachable!(),
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast.ge(right_ast));
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float { ast: left_ast, .. } => {
                    if let AstType::Float { ast: right_ast, .. } = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast.ge(right_ast));
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
            },
            BinOp::Gt => match left.ast_type() {
                AstType::Bool(_) | AstType::String(_) => unreachable!(),
                AstType::Int(left_ast) => {
                    if let AstType::Int(right_ast) = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast.gt(right_ast));
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
                AstType::Float { ast: left_ast, .. } => {
                    if let AstType::Float { ast: right_ast, .. } = right.ast_type() {
                        let result = z3::ast::Bool::new_const(&ctx.0, variable_name);
                        let formula = result._eq(&left_ast.gt(right_ast));
                        (AstType::Bool(result), vec![formula], left.ty_kind().clone())
                    } else {
                        unreachable!()
                    }
                }
            },
            BinOp::Offset => todo!(),
        };
        (
            symbolic_type,
            AstTypeAndFormulas(ast_type, formulas),
            ty_kind,
        )
    }

    pub fn create_ast_based_on_ty(
        &self,
        ctx: &'ctx Context,
        ty: TyKind,
        variable_name: String,
        serialized_const_value: Option<String>,
        source_operand: &Operand,
    ) -> AstTypeAndFormulas<'ctx> {
        // FIXME Formulas
        match ty {
            TyKind::Bool => {
                let ast = z3::ast::Bool::new_const(&ctx.0, variable_name.clone());

                let formulas = if let Some(serialized_const_value) = serialized_const_value {
                    vec![ast._eq(&z3::ast::Bool::from_bool(
                        &ctx.0,
                        bool::from_str(&serialized_const_value).unwrap(),
                    ))]
                } else {
                    let expr: Option<Arc<Expression>> = self.expr_from_operand(source_operand);
                    if let Some(expr) = expr {
                        if let AstType::Bool(operand_ast) = expr.ast_type() {
                            vec![ast._eq(operand_ast)]
                        } else {
                            vec![]
                        }
                    } else {
                        vec![]
                    }
                };

                AstTypeAndFormulas(AstType::Bool(ast), formulas)
            }
            TyKind::Char => todo!(),
            TyKind::Int(_) | TyKind::Uint(_) => {
                let ast = z3::ast::Int::new_const(&ctx.0, variable_name.clone());

                let formulas = if let Some(serialized_const_value) = serialized_const_value {
                    vec![ast._eq(&z3::ast::Int::from_str(&ctx.0, &serialized_const_value).unwrap())]
                } else {
                    let expr: Option<Arc<Expression>> = self.expr_from_operand(source_operand);
                    if let Some(expr) = expr {
                        if let AstType::Int(operand_ast) = expr.ast_type() {
                            vec![ast._eq(operand_ast)]
                        } else {
                            vec![]
                        }
                    } else {
                        vec![]
                    }
                };

                AstTypeAndFormulas(AstType::Int(ast), formulas)
            }
            TyKind::Float(FloatTy::F32) => todo!(),
            TyKind::Float(FloatTy::F64) => todo!(),
            TyKind::Adt => todo!(),
            TyKind::Foreign => todo!(),
            TyKind::Str => {
                let ast = z3::ast::String::new_const(&ctx.0, variable_name.clone());

                let formulas = if let Some(serialized_const_value) = serialized_const_value {
                    vec![ast
                        ._eq(&z3::ast::String::from_str(&ctx.0, &serialized_const_value).unwrap())]
                } else {
                    let expr: Option<Arc<Expression>> = self.expr_from_operand(source_operand);
                    if let Some(expr) = expr {
                        if let AstType::String(operand_ast) = expr.ast_type() {
                            vec![ast._eq(operand_ast)]
                        } else {
                            vec![]
                        }
                    } else {
                        vec![]
                    }
                };

                AstTypeAndFormulas(AstType::String(ast), formulas)
            }
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
            TyKind::Tuple(_) => todo!(),
            TyKind::Projection => todo!(),
            TyKind::Opaque => todo!(),
            TyKind::Param => todo!(),
            TyKind::Bound => todo!(),
            TyKind::Placeholder => todo!(),
            TyKind::Infer => todo!(),
            TyKind::Error => todo!(),
        }
    }

    pub fn create_type_ast_and_formulas_for_rvalue_use(
        &self,
        context: &'ctx Context,
        variable_name: String,
        operand: &Operand,
        serialized_const_value: Option<String>,
        ty: Option<TyKind>,
        debug_info: Option<&DebugInfo>,
    ) -> (SymbolicType<'ctx>, AstTypeAndFormulas<'ctx>, TyKind) {
        let ty = if let Some(ty) = ty {
            ty
        } else {
            let expr = self.expr_from_operand(operand);

            if let Some(expr) = expr {
                expr.ty_kind().clone()
            } else {
                if let Operand::Constant(b) = operand {
                    let constant = &**b;
                    constant.literal.get_ty().kind().clone()
                } else {
                    dbg!(&self);
                    panic!(
                        "missing type means operand {:?} should've been from existing place",
                        operand
                    )
                }
            }
        };

        // TODO: Handle Operand::Move by removing from the previous place and adding that expr
        //  as just the dependency.
        (
            self.operand_symbolic_type(operand, debug_info),
            self.create_ast_based_on_ty(
                context,
                ty.clone(),
                variable_name,
                serialized_const_value,
                operand,
            ),
            ty,
        )
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

        let (symbolic_type, ast_type_and_formulas, ty) = match &rvalue {
            Rvalue::Use(operand) => self.create_type_ast_and_formulas_for_rvalue_use(
                ctx,
                variable_name.clone(),
                operand,
                serialized_constant_value.as_ref().cloned(),
                ty,
                debug_info,
            ),
            Rvalue::Repeat(_, _) => todo!(),
            Rvalue::Ref(_, _) => todo!(),
            Rvalue::ThreadLocalRef => todo!(),
            Rvalue::AddressOf(_, _) => todo!(),
            Rvalue::Len(_) => todo!(),
            Rvalue::Cast(_, _, _) => todo!(),
            Rvalue::BinaryOp(b, operand_pair) => self.handle_binary_op_ast_creation(
                ctx,
                fn_name,
                destination,
                debug_info,
                b,
                operand_pair,
            ),
            Rvalue::CheckedBinaryOp(b, operand_pair) => {
                // TODO: Detect overflows?
                self.handle_binary_op_ast_creation(
                    ctx,
                    fn_name,
                    destination,
                    debug_info,
                    b,
                    operand_pair,
                )
            }
            Rvalue::NullaryOp(_, _) => todo!(),
            Rvalue::UnaryOp(_, _) => todo!(),
            Rvalue::Discriminant(_) => todo!(),
            Rvalue::Aggregate(_, _) => todo!(),
            Rvalue::ShallowInitBox(_, _) => todo!(),
        };
        self.insert_expr_with_symbolic_type(
            destination,
            symbolic_type,
            ast_type_and_formulas,
            ty,
            serialized_constant_value,
            variable_name,
        );
    }

    pub fn insert_expr_with_symbolic_type(
        &mut self,
        destination: &Place,
        symbolic_type: SymbolicType<'ctx>,
        ast_type_and_formulas: AstTypeAndFormulas<'ctx>,
        ty: TyKind,
        serialized_constant_value: Option<String>,
        variable_name: String,
    ) {
        match symbolic_type {
            SymbolicType::ExplicitSymbolic => {
                self.insert(
                    destination,
                    Expression::Symbolic {
                        place: destination.local,
                        ty,
                        ast_type_and_formulas,
                        variable_name,
                    },
                );
            }
            SymbolicType::Concrete { .. } if serialized_constant_value.is_some() => {
                self.insert(
                    destination,
                    Expression::ConcreteConstant {
                        place: destination.local,
                        ty,
                        ast_type_and_formulas,
                        serialized_value: serialized_constant_value.unwrap(),
                    },
                );
            }
            SymbolicType::InvolvesSymbolic { dependencies } => {
                self.insert(
                    destination,
                    Expression::Rvalue {
                        place: destination.local,
                        ty,
                        ast_type_and_formulas,
                        is_symbolic: true,
                        dependencies,
                    },
                );
            }
            SymbolicType::Concrete { dependencies } => {
                self.insert(
                    destination,
                    Expression::Rvalue {
                        place: destination.local,
                        ty,
                        ast_type_and_formulas,
                        is_symbolic: false,
                        dependencies,
                    },
                );
            }
        };
    }

    fn insert(&mut self, place: &Place, expression: Expression<'ctx>) {
        self.map.insert(place.local, Arc::new(expression));
    }

    fn contains(&self, place: &Place) -> bool {
        self.map.contains_key(&place.local)
    }

    fn remove(&mut self, place: &Place) -> Option<Arc<Expression>> {
        self.map.remove(&place.local)
    }
}

#[derive(Debug)]
struct SwitchIntInfo<'ctx> {
    discriminant: Arc<Expression<'ctx>>,
    basic_block_to_constraint_map: HashMap<u32, Bool<'ctx>>,
}

unsafe impl<'a> std::marker::Sync for SwitchIntInfo<'a> {}

unsafe impl<'a> std::marker::Send for SwitchIntInfo<'a> {}

#[derive(Debug)]
pub enum FunctionCallContext<'ctx> {
    NoReturn {
        function_name: Option<String>,
        level: usize,
        place_map: PlaceMap<'ctx>,
        switch_int_info: Option<SwitchIntInfo<'ctx>>,
    },
    WithReturn {
        function_name: Option<String>,
        level: usize,
        place_map: PlaceMap<'ctx>,
        switch_int_info: Option<SwitchIntInfo<'ctx>>,
        return_type: Ty,
        destination: PlaceAndDebugInfo,
    },
}

impl<'ctx> FunctionCallContext<'ctx> {
    pub fn function_name(&self) -> Option<&String> {
        match self {
            FunctionCallContext::NoReturn { function_name, .. }
            | FunctionCallContext::WithReturn { function_name, .. } => function_name.as_ref(),
        }
    }

    pub fn level(&self) -> usize {
        match self {
            FunctionCallContext::NoReturn { level, .. }
            | FunctionCallContext::WithReturn { level, .. } => *level,
        }
    }

    pub fn place_map(&self) -> &PlaceMap<'ctx> {
        match self {
            FunctionCallContext::NoReturn { place_map, .. }
            | FunctionCallContext::WithReturn { place_map, .. } => place_map,
        }
    }

    pub fn place_map_mut(&mut self) -> &mut PlaceMap<'ctx> {
        match self {
            FunctionCallContext::NoReturn { place_map, .. }
            | FunctionCallContext::WithReturn { place_map, .. } => place_map,
        }
    }

    pub fn switch_int_info_mut(&mut self) -> Option<&mut SwitchIntInfo<'ctx>> {
        match self {
            FunctionCallContext::NoReturn {
                switch_int_info, ..
            }
            | FunctionCallContext::WithReturn {
                switch_int_info, ..
            } => switch_int_info.as_mut(),
        }
    }

    pub fn update_switch_int_info(&mut self, new_switch_int_info: SwitchIntInfo<'ctx>) {
        match self {
            FunctionCallContext::NoReturn {
                switch_int_info, ..
            }
            | FunctionCallContext::WithReturn {
                switch_int_info, ..
            } => {
                switch_int_info.replace(new_switch_int_info);
            }
        }
    }
}

#[derive(Debug)]
struct PathConstraint<'ctx> {
    formula: Bool<'ctx>,
    discriminant: Arc<Expression<'ctx>>,
}

#[derive(Debug)]
pub struct FunctionCallStack<'ctx> {
    stack: Vec<FunctionCallContext<'ctx>>,
    path_constraints: Vec<PathConstraint<'ctx>>,
}

unsafe impl<'a> std::marker::Sync for FunctionCallStack<'a> {}

unsafe impl<'a> std::marker::Send for FunctionCallStack<'a> {}

fn construct_assumptions_from_dependencies<'a>(
    discriminant: &'a Arc<Expression<'a>>,
) -> Vec<&'a z3::ast::Bool<'a>> {
    fn add_formulas_recurse<'a>(
        formulas_list: &mut Vec<&'a z3::ast::Bool<'a>>,
        dependency: &'a Arc<Expression<'a>>,
    ) {
        for formula in &dependency.ast_type_and_formulas().1 {
            formulas_list.push(formula)
        }

        if let Expression::Rvalue { dependencies, .. } = &dependency.deref() {
            for dep in dependencies {
                add_formulas_recurse(formulas_list, dep);
            }
        }
    }
    let mut formulas_list = Vec::new();
    add_formulas_recurse(&mut formulas_list, discriminant);
    formulas_list
}

impl<'ctx> FunctionCallStack<'ctx> {
    pub fn new() -> Self {
        // TODO: Detect whether the main function has a return value or not
        Self {
            stack: vec![FunctionCallContext::NoReturn {
                function_name: Some(String::from("main")),
                level: 0,
                place_map: PlaceMap::new(),
                switch_int_info: None,
            }],
            path_constraints: vec![],
        }
    }

    fn do_mut(&mut self) {}

    pub fn process_switch_int_info(&mut self, current_basic_block: u32) {
        let ctx = self.current_ctx_mut().expect("context present");
        if let Some(switch_int_info) = ctx.switch_int_info_mut() {
            let discriminant = Arc::clone(&switch_int_info.discriminant);
            if let Some(path_constraint) = switch_int_info
                .basic_block_to_constraint_map
                .remove(&current_basic_block)
            {
                println!("Push path constraint {:?}", path_constraint);
                let path_constraint = PathConstraint {
                    formula: path_constraint,
                    discriminant,
                };
                self.path_constraints.push(path_constraint);
                self.current_ctx_mut()
                    .expect("context present")
                    .switch_int_info_mut()
                    .take();
            }
        }
    }

    pub fn handle_switch_int(
        &mut self,
        solver: &Solver<'ctx>,
        discriminant: Operand,
        switch_targets: SwitchTargets,
    ) {
        let solver = &solver.0;
        let discriminant = {
            let ctx = self.current_ctx_mut().unwrap();
            let place_map = ctx.place_map();
            let discriminant =
                if let Some(discriminant) = place_map.expr_from_operand(&discriminant) {
                    discriminant
                } else {
                    println!("Skipping SAT call: missing discriminant place AST node");
                    return;
                };
            if !discriminant.is_symbolic() {
                // TODO: Better detection. We should have discriminant.is_symbolic() == true if
                //  discriminant is a result of a path from a symbolic variable.
                if self.path_constraints.is_empty() {
                    println!("Skipping SAT call: discriminant is not symbolic");
                    return;
                }
            }
            discriminant
        };

        let mut formulas_and_path_constraints =
            construct_assumptions_from_dependencies(&discriminant);
        for path_constraint in &self.path_constraints {
            formulas_and_path_constraints.push(&path_constraint.formula);
            formulas_and_path_constraints.append(&mut construct_assumptions_from_dependencies(
                &path_constraint.discriminant,
            ))
        }

        let ast_expr = discriminant.ast_type();

        // TODO: Record values to use for alternate paths somewhere
        // TODO: When we reach a branch is selected, any values inside should use the
        //  discriminator formula as a "path constraint". This might require adding a new leafrt
        //  injected call at the start of every basic block
        let mut basic_block_to_constraint_map: HashMap<u32, Bool<'ctx>> = HashMap::new();
        let otherwise_basic_block = switch_targets.otherwise;
        match ast_expr {
            AstType::Bool(discriminant_bool) => {
                let value_targets = switch_targets
                    .switch_targets
                    .iter()
                    .map(|(value, target)| (*value == 1, *target));
                for (value, target) in value_targets {
                    for formula in &formulas_and_path_constraints {
                        solver.assert(formula);
                    }
                    let path_constraint =
                        discriminant_bool._eq(&Bool::from_bool(solver.get_context(), value));
                    solver.assert(&path_constraint);
                    basic_block_to_constraint_map.insert(target, path_constraint);
                    match solver.check() {
                        SatResult::Sat => {
                            println!("Reaching the {} branch is possible", value);
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
                    solver.reset();
                }
            }
            AstType::Int(discriminant_int) => {
                for (value, target) in switch_targets.switch_targets {
                    for formula in &formulas_and_path_constraints {
                        solver.assert(formula);
                    }
                    let path_constraint = discriminant_int
                        ._eq(&Int::from_str(solver.get_context(), &value.to_string()).unwrap());
                    solver.assert(&path_constraint);
                    basic_block_to_constraint_map.insert(target, path_constraint);
                    match solver.check() {
                        SatResult::Sat => {
                            println!("Reaching the {} branch is possible", value);
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
                    solver.reset();
                }
            }
            AstType::Float { .. } => todo!(),
            AstType::String(_) => todo!(),
        }
        if let Some(otherwise_basic_block) = otherwise_basic_block {
            let values = basic_block_to_constraint_map.values();
            match values.len() {
                0 => {}
                1 => {
                    let single = basic_block_to_constraint_map
                        .values()
                        .last()
                        .expect("1 element");
                    basic_block_to_constraint_map.insert(otherwise_basic_block, single.not());
                }
                _ => {
                    // If A, B, C are constraints for if and else-if, then to reach the else block, we need
                    // ~A & ~B & ~C; this is equivalent to ~(A || B || C)
                    let otherwise_constraint = Bool::or(
                        solver.get_context(),
                        &basic_block_to_constraint_map.values().collect::<Vec<_>>(),
                    )
                    .not();
                    basic_block_to_constraint_map
                        .insert(otherwise_basic_block, otherwise_constraint);
                }
            }
        }

        let switch_int_info = SwitchIntInfo {
            discriminant: discriminant,
            basic_block_to_constraint_map,
        };
        self.current_ctx_mut()
            .unwrap()
            .update_switch_int_info(switch_int_info);
    }

    pub fn handle_ret(&mut self, ctx: &'ctx Context, basic_block_idx: u32) {
        let mut context_of_returned_function = self.pop();
        println!(
            "[ret] bb{}",
            basic_block_idx,
        );
        if let Some(ref mut context_of_returned_function) = context_of_returned_function {
            if let FunctionCallContext::WithReturn {
                place_map,
                return_type,
                destination:
                    PlaceAndDebugInfo {
                        place: Some(destination),
                        debug_info,
                    },
                ..
            } = context_of_returned_function
            {
                let parent_caller_fn_name = self
                    .current_ctx()
                    .and_then(|ctx| ctx.function_name().cloned());
                let map_of_parent_caller = self.current_place_map_mut().unwrap();
                let variable_name = generate_variable_name(
                    parent_caller_fn_name.as_ref(),
                    destination,
                    debug_info.as_ref(),
                );
                let ast_type = match return_type.kind() {
                    TyKind::Bool => {
                        AstType::Bool(z3::ast::Bool::new_const(&ctx.0, variable_name.clone()))
                    }
                    TyKind::Char => todo!(),
                    TyKind::Int(_) | TyKind::Uint(_) => {
                        AstType::Int(z3::ast::Int::new_const(&ctx.0, variable_name.clone()))
                    }
                    TyKind::Float(FloatTy::F32) => AstType::Float {
                        ast: z3::ast::Float::new_const_float32(&ctx.0, variable_name.clone()),
                        is_f32: true,
                    },
                    TyKind::Float(FloatTy::F64) => AstType::Float {
                        ast: z3::ast::Float::new_const_double(&ctx.0, variable_name.clone()),
                        is_f32: false,
                    },
                    TyKind::Adt => todo!(),
                    TyKind::Foreign => todo!(),
                    TyKind::Str => {
                        AstType::String(z3::ast::String::new_const(&ctx.0, variable_name.clone()))
                    }
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
                    TyKind::Tuple(_) => todo!(),
                    TyKind::Projection => todo!(),
                    TyKind::Opaque => todo!(),
                    TyKind::Param => todo!(),
                    TyKind::Bound => todo!(),
                    TyKind::Placeholder => todo!(),
                    TyKind::Infer => todo!(),
                    TyKind::Error => todo!(),
                };

                if let Some(DebugInfo { name: Some(name) }) = debug_info {
                    if name.contains("leaf_symbolic") {
                        map_of_parent_caller.insert_expr(
                            &destination,
                            Arc::new(Expression::Symbolic {
                                place: destination.local,
                                ty: return_type.kind().clone(),
                                ast_type_and_formulas: AstTypeAndFormulas(ast_type, vec![]),
                                variable_name,
                            }),
                        );
                        return;
                    }
                }

                static LOCAL_PLACE: Local = Local { private: 0 };
                // Moving ownership. Any expressions constructed inside of the function are now
                // moved to the return destination.
                let expr_from_returned_function = place_map.map.remove(&LOCAL_PLACE).unwrap();

                let formulas = vec![match (&ast_type, expr_from_returned_function.ast_type()) {
                    (AstType::Bool(left), AstType::Bool(right)) => left._eq(right),
                    (AstType::Int(left), AstType::Int(right)) => left._eq(right),
                    (AstType::Float { ast: left, .. }, AstType::Float { ast: right, .. }) => {
                        left._eq(right)
                    }
                    (AstType::String(left), AstType::String(right)) => left._eq(right),
                    _ => unreachable!(),
                }];
                let destination_expr = Expression::Rvalue {
                    place: destination.local,
                    ty: return_type.kind().clone(),
                    is_symbolic: expr_from_returned_function.is_symbolic(),
                    ast_type_and_formulas: AstTypeAndFormulas(ast_type, formulas),
                    dependencies: vec![Arc::clone(&expr_from_returned_function)],
                };
                map_of_parent_caller.insert_expr(&destination, Arc::new(destination_expr));
            }
        }
    }

    pub fn handle_fn_call(
        &mut self,
        context: &'ctx Context,
        basic_block_idx: u32,
        function_debug_info: DebugInfo,
        func_return_type: Ty,
        return_destination: PlaceAndDebugInfo,
        args: OperandVec,
        const_arg_values: OperandConstValueVec,
    ) {
        self.process_switch_int_info(basic_block_idx);
        let mut place_map = PlaceMap::new();
        let parent_caller_place_map = self
            .current_place_map_mut()
            .expect("parent should have PlaceMap");

        for (idx, arg) in args.0.iter().enumerate() {
            let place = Place {
                local: Local {
                    private: (idx + 1) as u32,
                },
                projection: vec![],
            };

            let const_value = const_arg_values
                .0
                .get(idx)
                .expect("list have same size")
                .as_ref()
                .cloned();
            // dbg!(&rvalue);
            let variable_name =
                generate_variable_name(function_debug_info.name.as_ref(), &place, None);

            let (symbolic_type, ast_and_formulas, ty) = parent_caller_place_map
                .create_type_ast_and_formulas_for_rvalue_use(
                    context,
                    variable_name.clone(),
                    arg,
                    const_value.as_ref().cloned(),
                    None,
                    None,
                );

            dbg!(&symbolic_type);

            place_map.insert_expr_with_symbolic_type(
                &place,
                symbolic_type,
                ast_and_formulas,
                ty,
                const_value,
                variable_name,
            )
            /*
            let (symbolic_type, ast_type_and_formulas, ty) = {
                let const_value = const_arg_values.0
                    .get(idx)
                    .expect("list have same size")
                    .as_ref()
                    .cloned();

                parent_caller_place_map.create_type_ast_and_formulas_for_rvalue_use(
                    context,
                    generate_variable_name(
                        function_debug_info.name.as_ref(),
                        &place,
                        None
                    ),
                    arg,
                    const_value,
                    None,
                    None
                )
            };

             */
        }

        let level = self.stack.len();
        // When a Rust function returns nothing, it actually returns the unit, ()---an empty tuple.
        let is_unit_return = match func_return_type.kind() {
            TyKind::Tuple(type_vec) => type_vec.is_empty(),
            _ => false,
        };
        let switch_int_info = None;
        if return_destination.place.is_some() && !is_unit_return {
            self.stack.push(FunctionCallContext::WithReturn {
                function_name: function_debug_info.name,
                level,
                place_map,
                switch_int_info,
                return_type: func_return_type,
                destination: return_destination,
            })
        } else {
            self.stack.push(FunctionCallContext::NoReturn {
                function_name: function_debug_info.name,
                level,
                place_map,
                switch_int_info,
            })
        }
    }

    pub fn handle_assign(
        &mut self,
        context: &'ctx Context,
        basic_block_idx: u32,
        place_and_debug_info: &PlaceAndDebugInfo,
        rvalue: Option<&Rvalue>,
        ty: Option<TyKind>,
        serialized_const_value: Option<String>,
    ) {
        self.process_switch_int_info(basic_block_idx);
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
                SerializedValue(kind, Some(input.to_string()))
            }

            pub fn [< as_ $t >](&self) -> Option<$t> {
                self.1.as_ref().and_then(|s| $t::from_str(&s).ok())
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
    Float {
        ast: z3::ast::Float<'ctx>,
        is_f32: bool,
    },
    String(z3::ast::String<'ctx>),
}

unsafe impl<'a> std::marker::Sync for AstType<'a> {}

unsafe impl<'a> std::marker::Send for AstType<'a> {}

#[derive(Debug)]
struct AstTypeAndFormulas<'ctx>(AstType<'ctx>, Vec<z3::ast::Bool<'ctx>>);

unsafe impl<'a> std::marker::Sync for AstTypeAndFormulas<'a> {}

unsafe impl<'a> std::marker::Send for AstTypeAndFormulas<'a> {}

#[derive(Debug)]
pub enum Expression<'ctx> {
    Symbolic {
        place: Local,
        ty: TyKind,
        ast_type_and_formulas: AstTypeAndFormulas<'ctx>,
        variable_name: String,
    },
    ConcreteConstant {
        place: Local,
        ty: TyKind,
        ast_type_and_formulas: AstTypeAndFormulas<'ctx>,
        serialized_value: String,
    },
    Rvalue {
        place: Local,
        ast_type_and_formulas: AstTypeAndFormulas<'ctx>,
        ty: TyKind,
        /// Whether any of the nodes inside of [ast_type] is symbolic.
        is_symbolic: bool,
        dependencies: Vec<Arc<Expression<'ctx>>>,
    },
}

impl<'ctx> Expression<'ctx> {
    fn local_place(&self) -> &Local {
        match self {
            Expression::Symbolic { place, .. }
            | Expression::ConcreteConstant { place, .. }
            | Expression::Rvalue { place, .. } => place,
        }
    }

    fn ty_kind(&self) -> &TyKind {
        match self {
            Expression::Symbolic { ty, .. } => ty,
            Expression::ConcreteConstant { ty, .. } => ty,
            Expression::Rvalue { ty, .. } => ty,
        }
    }

    fn ast_type_and_formulas(&self) -> &AstTypeAndFormulas<'ctx> {
        match self {
            Expression::Symbolic {
                ast_type_and_formulas,
                ..
            } => &ast_type_and_formulas,
            Expression::ConcreteConstant {
                ast_type_and_formulas,
                ..
            } => &ast_type_and_formulas,
            Expression::Rvalue {
                ast_type_and_formulas,
                ..
            } => &ast_type_and_formulas,
        }
    }

    fn ast_type(&self) -> &AstType<'ctx> {
        match self {
            Expression::Symbolic {
                ast_type_and_formulas,
                ..
            } => &ast_type_and_formulas.0,
            Expression::ConcreteConstant {
                ast_type_and_formulas,
                ..
            } => &ast_type_and_formulas.0,
            Expression::Rvalue {
                ast_type_and_formulas,
                ..
            } => &ast_type_and_formulas.0,
        }
    }

    fn is_symbolic(&self) -> bool {
        match self {
            Expression::Symbolic { .. } => true,
            Expression::ConcreteConstant { .. } => false,
            Expression::Rvalue { is_symbolic, .. } => *is_symbolic,
        }
    }

    fn symbolic_type(&self) -> SymbolicType<'ctx> {
        match self {
            Expression::Symbolic { .. } => SymbolicType::ExplicitSymbolic,
            Expression::ConcreteConstant { .. } => SymbolicType::Concrete {
                dependencies: vec![],
            },
            Expression::Rvalue {
                is_symbolic,
                dependencies,
                ..
            } => {
                if *is_symbolic {
                    SymbolicType::InvolvesSymbolic {
                        dependencies: dependencies.clone(),
                    }
                } else {
                    SymbolicType::Concrete {
                        dependencies: dependencies.clone(),
                    }
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
