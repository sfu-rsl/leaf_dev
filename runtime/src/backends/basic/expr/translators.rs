pub(crate) mod z3 {
    use std::{
        collections::HashMap,
        mem::{discriminant, size_of},
        ops::Not,
    };

    use z3::{
        ast::{self, Ast},
        Context,
    };

    use crate::{
        abs::{BinaryOp, UnaryOp},
        backends::basic::expr::{
            ConcreteValue, ConstValue, Expr, SymBinaryOperands, SymValue, SymVarId, SymbolicVar,
            SymbolicVarType, Value, ValueRef,
        },
    };

    use crate::solvers::z3::{AstNode, AstPair};

    const CHAR_BIT_SIZE: u32 = size_of::<char>() as u32 * 8;
    const TO_CHAR_BIT_SIZE: u32 = 8; // Can only cast to a char from a u8

    impl<'ctx> From<(&ValueRef, &'ctx Context)> for AstPair<'ctx, SymVarId> {
        fn from(value_with_context: (&ValueRef, &'ctx Context)) -> Self {
            let (value, context) = value_with_context;
            let mut translator = Z3ValueTranslator::new(context);
            translator.translate(value)
        }
    }

    pub(crate) struct Z3ValueTranslator<'ctx> {
        context: &'ctx Context,
        variables: HashMap<SymVarId, AstNode<'ctx>>,
    }

    impl<'ctx> Z3ValueTranslator<'ctx> {
        pub(crate) fn new(context: &'ctx Context) -> Self {
            Self {
                context,
                variables: HashMap::new(),
            }
        }
    }

    impl<'ctx> Z3ValueTranslator<'ctx> {
        fn translate(&mut self, value: &ValueRef) -> AstPair<'ctx, SymVarId> {
            let ast = self.translate_value(value);
            match ast {
                AstNode::Bool(ast) => AstPair(ast, self.variables.drain().collect()),
                _ => panic!("Expected the value to be a boolean expression but it is a {ast:#?}.",),
            }
        }
    }

    impl<'ctx> Z3ValueTranslator<'ctx> {
        fn translate_value(&mut self, value: &ValueRef) -> AstNode<'ctx> {
            match value.as_ref() {
                Value::Concrete(c) => self.translate_concrete(c),
                Value::Symbolic(s) => self.translate_symbolic(s),
            }
        }

        fn translate_concrete(&mut self, concrete: &ConcreteValue) -> AstNode<'ctx> {
            match concrete {
                ConcreteValue::Const(c) => self.translate_const(c),
                ConcreteValue::Adt(_) => {
                    unimplemented!("Expressions involving ADTs directly are not supported.")
                }
                ConcreteValue::Array(_) => todo!(),
                ConcreteValue::Ref(_) => todo!(),
            }
        }

        fn translate_const(&mut self, const_value: &ConstValue) -> AstNode<'ctx> {
            match const_value {
                ConstValue::Bool(b) => ast::Bool::from_bool(self.context, *b).into(),
                ConstValue::Char(c) => {
                    AstNode::from_ubv(ast::BV::from_u64(self.context, *c as u64, CHAR_BIT_SIZE))
                }
                ConstValue::Int {
                    bit_rep,
                    size,
                    is_signed: false,
                } => {
                    // TODO: Add support for 128 bit integers.
                    AstNode::from_bv(
                        ast::BV::from_u64(
                            self.context,
                            *bit_rep as u64,
                            (*size).try_into().expect("Size is too large."),
                        ),
                        false,
                    )
                }
                ConstValue::Int {
                    bit_rep,
                    size,
                    is_signed: true,
                } => {
                    // TODO: Add support for 128 bit integers.
                    AstNode::from_bv(
                        ast::BV::from_i64(
                            self.context,
                            *bit_rep as i64,
                            (*size).try_into().expect("Size is too large."),
                        ),
                        true,
                    )
                }
                ConstValue::Float { .. } => todo!(),
                ConstValue::Str(_) => todo!(),
                ConstValue::Func(_) => todo!(),
            }
        }

        fn translate_symbolic(&mut self, symbolic: &SymValue) -> AstNode<'ctx> {
            match symbolic {
                SymValue::Variable(var) => self.translate_symbolic_var(var),
                SymValue::Expression(expr) => self.translate_symbolic_expr(expr),
            }
        }

        fn translate_symbolic_var(&mut self, var: &SymbolicVar) -> AstNode<'ctx> {
            self.translate_symbolic_var_and_record(var)
        }

        fn translate_symbolic_var_and_record(&mut self, var: &SymbolicVar) -> AstNode<'ctx> {
            let node = match var.ty {
                SymbolicVarType::Bool => ast::Bool::new_const(self.context, var.id).into(),
                SymbolicVarType::Char => {
                    AstNode::from_ubv(ast::BV::new_const(self.context, var.id, CHAR_BIT_SIZE))
                }
                SymbolicVarType::Int { size, is_signed } => AstNode::from_bv(
                    ast::BV::new_const(self.context, var.id, size as u32),
                    is_signed,
                ),
                SymbolicVarType::Float { .. } => todo!(),
            };
            self.variables.insert(var.id, node.clone());
            node
        }

        fn translate_symbolic_expr(&mut self, expr: &Expr) -> AstNode<'ctx> {
            match expr {
                Expr::Unary { operator, operand } => {
                    let operand = self.translate_symbolic(operand);
                    self.translate_unary_expr(operator, operand)
                }
                Expr::Binary { operator, operands } => {
                    let (left, right) = match operands {
                        SymBinaryOperands::Orig { first, second } => {
                            (self.translate_symbolic(first), self.translate_value(second))
                        }
                        SymBinaryOperands::Rev { first, second } => {
                            (self.translate_value(first), self.translate_symbolic(second))
                        }
                    };
                    self.translate_binary_expr(operator, left, right)
                }
                Expr::Cast { from, to } => {
                    let from = self.translate_symbolic(from);
                    self.translate_cast_expr(from, to)
                }
                Expr::AddrOf() => todo!(),
                Expr::Len { .. } => todo!(),
                Expr::Projection(_) => todo!(),
            }
        }

        fn translate_unary_expr(
            &mut self,
            operator: &UnaryOp,
            operand: AstNode<'ctx>,
        ) -> AstNode<'ctx> {
            match operator {
                UnaryOp::Not => match operand {
                    AstNode::Bool(ast) => ast.not().into(),
                    AstNode::BitVector { ast, is_signed } => {
                        AstNode::from_bv(ast.bvnot(), is_signed)
                    }
                    _ => unreachable!("Not is only supposed to be applied to bools and ints."),
                },
                UnaryOp::Neg => match operand {
                    AstNode::BitVector {
                        ast,
                        is_signed: true,
                    } => AstNode::from_bv(ast.bvneg(), true),
                    _ => unreachable!("Neg is only supposed to be applied to signed numbers."),
                },
            }
        }

        fn translate_binary_expr(
            &mut self,
            operator: &BinaryOp,
            left: AstNode<'ctx>,
            right: AstNode<'ctx>,
        ) -> AstNode<'ctx> {
            assert_eq!(discriminant(&left), discriminant(&right));

            match left {
                AstNode::Bool(_) => {
                    let left = left.as_bool();
                    let right = right.as_bool();
                    match operator {
                        BinaryOp::Eq => ast::Bool::_eq(left, right),
                        BinaryOp::Ne => ast::Bool::_eq(left, right).not(),
                        BinaryOp::BitAnd => ast::Bool::and(left.get_ctx(), &[left, right]),
                        BinaryOp::BitOr => ast::Bool::or(left.get_ctx(), &[left, right]),
                        BinaryOp::BitXor => ast::Bool::xor(left, right),
                        _ => unreachable!("Bool can only used with logical operators."),
                    }
                    .into()
                }
                AstNode::BitVector { is_signed, .. } => {
                    let left = left.as_bit_vector();
                    let right = right.as_bit_vector();
                    let ar_func: Option<fn(&ast::BV<'ctx>, &ast::BV<'ctx>) -> ast::BV<'ctx>> =
                        match (operator, is_signed) {
                            (BinaryOp::Add, _) => Some(ast::BV::bvadd),
                            (BinaryOp::Sub, _) => Some(ast::BV::bvsub),
                            (BinaryOp::Mul, _) => Some(ast::BV::bvmul),
                            (BinaryOp::Div, true) => Some(ast::BV::bvsdiv),
                            (BinaryOp::Div, false) => Some(ast::BV::bvudiv),
                            (BinaryOp::Rem, true) => Some(ast::BV::bvsrem),
                            (BinaryOp::Rem, false) => Some(ast::BV::bvurem),
                            (BinaryOp::BitXor, _) => Some(ast::BV::bvxor),
                            (BinaryOp::BitAnd, _) => Some(ast::BV::bvand),
                            (BinaryOp::BitOr, _) => Some(ast::BV::bvor),
                            (BinaryOp::Shl, _) => Some(ast::BV::bvshl),
                            /* Shift right function obtained from documentation
                             * https://doc.rust-lang.org/reference/expressions/operator-expr.html#arithmetic-and-logical-binary-operators
                             */
                            (BinaryOp::Shr, true) => Some(ast::BV::bvashr),
                            (BinaryOp::Shr, false) => Some(ast::BV::bvlshr),
                            _ => None,
                        };

                    if let Some(func) = ar_func {
                        AstNode::from_bv(func(left, right), is_signed)
                    } else {
                        let logical_func: fn(&ast::BV<'ctx>, &ast::BV<'ctx>) -> ast::Bool<'ctx> =
                            match (operator, is_signed) {
                                (BinaryOp::Eq, _) => ast::BV::_eq,
                                (BinaryOp::Ne, _) => |l, r| ast::BV::_eq(l, r).not(),
                                (BinaryOp::Lt, true) => ast::BV::bvslt,
                                (BinaryOp::Lt, false) => ast::BV::bvult,
                                (BinaryOp::Le, true) => ast::BV::bvsle,
                                (BinaryOp::Le, false) => ast::BV::bvule,
                                (BinaryOp::Ge, true) => ast::BV::bvsge,
                                (BinaryOp::Ge, false) => ast::BV::bvuge,
                                (BinaryOp::Gt, true) => ast::BV::bvsgt,
                                (BinaryOp::Gt, false) => ast::BV::bvugt,
                                (BinaryOp::Offset, _) => todo!(),
                                _ => unreachable!(),
                            };
                        logical_func(left, right).into()
                    }
                }
            }
        }

        fn translate_cast_expr(
            &mut self,
            from: AstNode<'ctx>,
            to: &SymbolicVarType,
        ) -> AstNode<'ctx> {
            match to {
                SymbolicVarType::Char => {
                    let from = from.as_bit_vector();
                    let size = from.get_size();
                    debug_assert!(
                        size == TO_CHAR_BIT_SIZE,
                        "Cast from {size} to char is not supported."
                    );
                    let ast = from.zero_ext(CHAR_BIT_SIZE - TO_CHAR_BIT_SIZE);
                    AstNode::from_bv(ast, false)
                }
                SymbolicVarType::Int { size, is_signed } => {
                    let size = *size as u32;
                    match from {
                        AstNode::Bool(_) => {
                            let from = from.as_bool();
                            let ast = if *is_signed {
                                from.ite(
                                    &ast::BV::from_i64(from.get_ctx(), 1, size),
                                    &ast::BV::from_i64(from.get_ctx(), 0, size),
                                )
                            } else {
                                from.ite(
                                    &ast::BV::from_u64(from.get_ctx(), 1, size),
                                    &ast::BV::from_u64(from.get_ctx(), 0, size),
                                )
                            };
                            AstNode::from_bv(ast, *is_signed)
                        }
                        AstNode::BitVector { ast, is_signed } => {
                            let old_size = ast.get_size();
                            if size > old_size {
                                let bits_to_add = size - old_size;
                                let ast = if is_signed {
                                    ast.sign_ext(bits_to_add)
                                } else {
                                    ast.zero_ext(bits_to_add)
                                };
                                AstNode::from_bv(ast, is_signed)
                            } else {
                                // This also handles the case where size == old_size since all bits will be extracted
                                // and the sign will be updated.
                                AstNode::from_bv(ast.extract(size - 1, 0), is_signed)
                            }
                        }
                    }
                }
                SymbolicVarType::Float { .. } => todo!(),
                _ => unreachable!("Casting from int to {to:#?} is not supported."),
            }
        }
    }

    impl<'ctx> From<AstNode<'ctx>> for ValueRef {
        fn from(ast: AstNode<'ctx>) -> Self {
            match ast {
                AstNode::Bool(ast) => {
                    super::super::Value::Concrete(super::super::ConcreteValue::Const(
                        super::super::ConstValue::Bool(ast.as_bool().unwrap()),
                    ))
                    .into()
                }
                AstNode::BitVector { ast, is_signed } => {
                    // TODO: Add support for up to 128-bit integers.
                    let value = if is_signed {
                        let bytes = ast.as_i64().unwrap().to_be_bytes();
                        let mut extended = [0_u8; 16];
                        extended[8..].copy_from_slice(&bytes);
                        u128::from_be_bytes(extended)
                    } else {
                        ast.as_u64().unwrap() as u128
                    };
                    ValueRef::new(super::super::Value::Concrete(
                        super::super::ConcreteValue::Const(super::super::ConstValue::Int {
                            is_signed,
                            bit_rep: value,
                            size: ast.get_size() as u64,
                        }),
                    ))
                }
            }
        }
    }
}