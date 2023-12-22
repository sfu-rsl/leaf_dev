pub(crate) mod z3 {
    use std::{
        collections::HashMap,
        default::Default,
        mem::{discriminant, size_of},
        ops::Not,
    };

    use z3::{
        ast::{self, Ast},
        Context,
    };

    use crate::{
        abs::{expr::sym_place::SelectTarget, BinaryOp, FieldIndex, IntType, UnaryOp, ValueType},
        backends::basic::expr::{
            prelude::*,
            sym_place::{
                apply_address_of, apply_len, DefaultProjExprReadResolver, ProjExprResolver, Select,
                SingleProjResult, SymbolicProjResult, TransmutedValue,
            },
            SymBinaryOperands, SymVarId,
        },
        solvers::z3::{ArrayNode, ArraySort, BVNode, BVSort},
    };

    use crate::solvers::z3::{AstNode, TranslatedConstraint};

    const CHAR_BIT_SIZE: u32 = size_of::<char>() as u32 * 8;
    const USIZE_BIT_SIZE: u32 = size_of::<usize>() as u32 * 8;
    const TO_CHAR_BIT_SIZE: u32 = 8; // Can only cast to a char from a u8

    const POSSIBLE_VALUES_PREFIX: &str = "pvs";

    impl<'ctx> From<(&ValueRef, &'ctx Context)> for TranslatedConstraint<'ctx, SymVarId> {
        fn from(value_with_context: (&ValueRef, &'ctx Context)) -> Self {
            let (value, context) = value_with_context;
            Z3ValueTranslator::new(context).translate(value)
        }
    }

    pub(crate) struct Z3ValueTranslator<'ctx> {
        context: &'ctx Context,
        variables: HashMap<SymVarId, AstNode<'ctx>>,
        constraints: Vec<ast::Bool<'ctx>>,
    }

    impl<'ctx> Z3ValueTranslator<'ctx> {
        pub(crate) fn new(context: &'ctx Context) -> Self {
            Self {
                context,
                variables: Default::default(),
                // Additional constraints are not common.
                constraints: Vec::with_capacity(0),
            }
        }
    }

    impl<'ctx> Z3ValueTranslator<'ctx> {
        fn translate(mut self, value: &ValueRef) -> TranslatedConstraint<'ctx, SymVarId> {
            log::debug!("Translating value: {}", value);
            let ast = self.translate_value(value);
            match ast {
                AstNode::Bool(ast) => TranslatedConstraint {
                    constraint: ast,
                    variables: self.variables,
                    extra: self.constraints,
                },
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
                ConcreteValue::Array(array) => AstNode::Array(self.translate_array(array)),
                ConcreteValue::Ref(_) => todo!(),
                ConcreteValue::Unevaluated(_) => {
                    panic!("Unevaluated value should not exist at this phase.")
                }
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
                    ty:
                        IntType {
                            bit_size,
                            is_signed: false,
                        },
                } => {
                    // TODO: Add support for 128 bit integers.
                    let ast = ast::BV::from_u64(
                        self.context,
                        bit_rep.0 as u64,
                        (*bit_size).try_into().expect("Size is too large."),
                    );
                    BVNode::new(ast, false).into()
                }
                ConstValue::Int {
                    bit_rep,
                    ty:
                        IntType {
                            bit_size,
                            is_signed: true,
                        },
                } => {
                    // TODO: Add support for 128 bit integers.
                    let ast = ast::BV::from_i64(
                        self.context,
                        bit_rep.0 as i64,
                        (*bit_size).try_into().expect("Size is too large."),
                    );
                    BVNode::new(ast, true).into()
                }
                ConstValue::Float { .. } => todo!(),
                ConstValue::Str(_) => todo!(),
                ConstValue::Func(_) => unreachable!(concat!(
                    "Function values are not supposed to appear in symbolic expressions.",
                    "Symbolic function pointers are not expected to appear as function constants."
                )),
                ConstValue::Zst => unreachable!(
                    "Zero-sized-typed values are not supposed to appear in symbolic expressions."
                ),
            }
        }

        fn translate_array(&mut self, array: &ArrayValue) -> ArrayNode<'ctx> {
            self.translate_array_of_values("arr", array.elements.iter(), Self::translate_value)
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
                ValueType::Bool => ast::Bool::new_const(self.context, var.id).into(),
                ValueType::Char => {
                    AstNode::from_ubv(ast::BV::new_const(self.context, var.id, CHAR_BIT_SIZE))
                }
                ValueType::Int(IntType {
                    bit_size,
                    is_signed,
                }) => {
                    let ast = ast::BV::new_const(self.context, var.id, bit_size as u32);
                    BVNode::new(ast, is_signed).into()
                }
                ValueType::Float { .. } => todo!(),
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
                Expr::Binary(BinaryExpr {
                    operator,
                    operands,
                    checked,
                }) => {
                    // Only projections care about if a binary operation is checked or not.
                    // A checked binary expression without a field projection is not well formed (before MIR optimizations are run).
                    assert!(!checked, "translating unexpected checked operation");
                    let (left, right) = self.translate_binary_operands(operands);
                    self.translate_binary_expr(operator, left, right)
                }
                Expr::Cast { from, to } => {
                    let from = self.translate_symbolic(from);
                    self.translate_cast_expr(from, to)
                }
                Expr::Extension {
                    source,
                    is_zero_ext,
                    bits_to_add,
                    is_signed,
                } => {
                    let source = self.translate_symbolic(source);
                    self.translate_extension_expr(source, is_zero_ext, bits_to_add, is_signed)
                }
                Expr::Extraction {
                    source,
                    high,
                    low,
                    is_signed,
                } => {
                    let source = self.translate_symbolic(source);
                    self.translate_extraction_expr(source, high, low, is_signed)
                }
                Expr::Ite {
                    condition,
                    if_target,
                    else_target,
                } => {
                    let condition = self.translate_symbolic(condition);
                    let if_target = self.translate_value(if_target);
                    let else_target = self.translate_value(else_target);
                    self.translate_ite_expr(condition, if_target, else_target)
                }
                Expr::AddrOf(operand) => {
                    let operand = self.resolve_proj_expression(operand);
                    self.translate_address_of_expr(operand)
                }
                Expr::Len(of) => {
                    let of = self.resolve_proj_expression(of);
                    self.translate_len_expr(of)
                }
                Expr::Projection(proj_expr) => self.translate_projection_expr(proj_expr),
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
                    AstNode::BitVector(bv) => bv.map(ast::BV::bvnot).into(),
                    _ => unreachable!("Not is not supported for this operand: {operand:#?}"),
                },
                UnaryOp::Neg => match operand {
                    AstNode::BitVector(bv @ BVNode(_, BVSort { is_signed: true })) => {
                        bv.map(ast::BV::bvneg).into()
                    }
                    _ => unreachable!("Neg is not supported for this operand: {operand:#?}"),
                },
            }
        }

        fn translate_binary_operands(
            &mut self,
            operands: &SymBinaryOperands,
        ) -> (AstNode<'ctx>, AstNode<'ctx>) {
            match operands {
                SymBinaryOperands::Orig { first, second } => {
                    (self.translate_symbolic(first), self.translate_value(second))
                }
                SymBinaryOperands::Rev { first, second } => {
                    (self.translate_value(first), self.translate_symbolic(second))
                }
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
                AstNode::BitVector(ref left_node) => {
                    let right = match operator {
                        BinaryOp::Shl | BinaryOp::Shr if left.z3_sort() != right.z3_sort() => {
                            // This cast may truncate `right`, but since the smallest sort is 8 bits, the largest value
                            // is 127, which is equal to the largest valid left or right shift of 127 for 128 bit values,
                            // so everything works out!
                            self.translate_cast_expr(
                                right,
                                // right must always be positive, so we can do an unsigned cast
                                &ValueType::new_int(left_node.size() as u64, false),
                            )
                        }
                        _ => right,
                    };
                    debug_assert_eq!(left.z3_sort(), right.z3_sort());
                    let right_bv = right.as_bit_vector();
                    let is_signed = left_node.is_signed();
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
                        left_node.map(|left| func(left, right_bv)).into()
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
                        logical_func(&left_node.0, right_bv).into()
                    }
                }
                _ => unreachable!("Binary expressions are not supported for this type: {left:#?}"),
            }
        }

        fn translate_cast_expr(&mut self, from: AstNode<'ctx>, to: &ValueType) -> AstNode<'ctx> {
            match to {
                ValueType::Char => {
                    let from = from.as_bit_vector();
                    let size = from.get_size();
                    debug_assert!(
                        size == TO_CHAR_BIT_SIZE,
                        "Cast from {size} to char is not supported."
                    );
                    BVNode::new(from.zero_ext(CHAR_BIT_SIZE - TO_CHAR_BIT_SIZE), false).into()
                }
                ValueType::Int(IntType {
                    bit_size,
                    is_signed,
                }) => {
                    let size = *bit_size as u32;
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
                            BVNode::new(ast, *is_signed).into()
                        }
                        AstNode::BitVector(BVNode(
                            ast,
                            BVSort {
                                is_signed: is_from_signed,
                            },
                        )) => {
                            let old_size = ast.get_size();
                            if size > old_size {
                                let bits_to_add = size - old_size;
                                let ast = if is_from_signed {
                                    ast.sign_ext(bits_to_add)
                                } else {
                                    ast.zero_ext(bits_to_add)
                                };
                                BVNode::new(ast, *is_signed).into()
                            } else {
                                // This also handles the case where size == old_size since all bits will be extracted
                                // and the sign will be updated.
                                BVNode::new(ast.extract(size - 1, 0), *is_signed).into()
                            }
                        }
                        _ => unreachable!("Casting from {from:#?} to int is not supported."),
                    }
                }
                ValueType::Float { .. } => todo!(),
                _ => unreachable!("Casting from int to {to:#?} is not supported."),
            }
        }

        fn translate_extension_expr(
            &mut self,
            source: AstNode<'ctx>,
            is_zero_ext: &bool,
            bits_to_add: &u32,
            is_signed: &bool,
        ) -> AstNode<'ctx> {
            match source {
                AstNode::BitVector(BVNode(ast, _)) => {
                    let ast = if *is_zero_ext {
                        ast.zero_ext(*bits_to_add)
                    } else {
                        ast.sign_ext(*bits_to_add)
                    };
                    BVNode::new(ast, *is_signed).into()
                }
                _ => unreachable!("Invalid extension expression for {:?}.", source),
            }
        }

        fn translate_extraction_expr(
            &mut self,
            source: AstNode<'ctx>,
            high: &u32,
            low: &u32,
            is_signed: &bool,
        ) -> AstNode<'ctx> {
            match source {
                AstNode::BitVector(BVNode(ast, _)) => {
                    BVNode::new(ast.extract(*high, *low), *is_signed).into()
                }
                _ => unreachable!("Invalid extraction expression for {:?}.", source),
            }
        }

        fn translate_ite_expr(
            &mut self,
            condition: AstNode<'ctx>,
            if_target: AstNode<'ctx>,
            else_target: AstNode<'ctx>,
        ) -> AstNode<'ctx> {
            match condition {
                AstNode::Bool(_) => {
                    let condition = condition.as_bool();
                    let ast = condition.ite(&if_target.ast(), &else_target.ast());
                    if ast == if_target.ast() {
                        AstNode::from_ast(ast, &if_target.sort())
                    } else {
                        AstNode::from_ast(ast, &else_target.sort())
                    }
                }
                _ => unimplemented!("Invalid ITE expression for {:?}", condition),
            }
        }

        fn translate_address_of_expr(&mut self, operand: SymbolicProjResult) -> AstNode<'ctx> {
            let result = apply_address_of(operand, &mut DefaultProjExprReadResolver);
            const ADDRESS_OF_VALUES_PREFIX: &str = "address_of";
            self.translate_symbolic_proj_result(&result, Some(ADDRESS_OF_VALUES_PREFIX))
        }

        fn translate_len_expr(&mut self, of: SymbolicProjResult) -> AstNode<'ctx> {
            let result = apply_len(of, &mut DefaultProjExprReadResolver);
            const LEN_VALUES_PREFIX: &str = "len";
            self.translate_symbolic_proj_result(&result, Some(LEN_VALUES_PREFIX))
        }

        fn translate_projection_expr(&mut self, proj_expr: &ProjExpr) -> AstNode<'ctx> {
            if let ProjExpr::SymHost(sym_host) = proj_expr {
                if let (
                    SymValue::Expression(Expr::Binary(BinaryExpr {
                        operator,
                        operands,
                        checked,
                    })),
                    ProjKind::Field(field_index),
                ) = (sym_host.host.as_ref(), &sym_host.kind)
                {
                    assert!(checked, "unexpected field projection on unchecked binop");
                    return self.translate_field_on_checked_binary_expression(
                        *operator,
                        operands,
                        *field_index,
                    );
                }
            }

            let proj_result = self.resolve_proj_expression(proj_expr);
            self.translate_symbolic_proj_result(&proj_result, None)
        }

        fn translate_select(
            &mut self,
            select: &Select,
            const_prefix: Option<&str>,
        ) -> AstNode<'ctx> {
            let index = self.translate_symbolic(&select.index.index);
            let index = if select.index.from_end {
                let len = match &select.target {
                    SelectTarget::Array(possible_values) => {
                        self.translate_const(&possible_values.len().into())
                    }
                    SelectTarget::Nested(box select) => {
                        self.translate_len_expr(
                            /* FIXME: May be expensive */
                            select.clone().into(),
                        )
                    }
                };
                self.translate_binary_expr(&BinaryOp::Sub, len, index)
            } else {
                index
            };
            debug_assert_eq!(
                index.z3_sort(),
                z3::Sort::bitvector(self.context, USIZE_BIT_SIZE)
            );

            /* NOTE: Do we need to add constraint that index is within bounds?
             * This code is meant for safe Rust. Thus,
             * Bound constraints are automatically implied by the bound checks compiler adds.
             * Also, we don't need to worry about the empty arrays for the same reason. */

            match &select.target {
                SelectTarget::Array(possible_values) => {
                    let ArrayNode(
                        ast,
                        ArraySort {
                            range: box elem_sort,
                        },
                    ) = self.translate_array_of_values(
                        const_prefix.unwrap_or(POSSIBLE_VALUES_PREFIX),
                        possible_values.iter(),
                        |this, r| this.translate_symbolic_proj_result(r, const_prefix),
                    );

                    let result =
                        AstNode::from_ast(ast::Array::select(&ast, &index.ast()), &elem_sort);
                    result
                }
                SelectTarget::Nested(box inner) => {
                    let inner = self.translate_select(inner, const_prefix);
                    if let AstNode::Array(ArrayNode(array, ArraySort { range: range_sort })) = inner
                    {
                        AstNode::from_ast(ast::Array::select(&array, &index.ast()), &range_sort)
                    } else {
                        unreachable!("Nested select result should be an array.")
                    }
                }
            }
        }

        fn translate_array_of_values<'a, V: 'a>(
            &mut self,
            const_prefix: &str,
            values: impl Iterator<Item = &'a V>,
            translate: impl Fn(&mut Self, &V) -> AstNode<'ctx>,
        ) -> ArrayNode<'ctx>
        where
            'ctx: 'a,
        {
            let context = self.context;

            let mut values = values.map(|v| translate(self, v));
            let first = values
                .next()
                .expect("Indices on zero-sized arrays should be prevented by the bound checks.");
            let element_sort = first.sort().clone();

            let mut array = ast::Array::fresh_const(
                context,
                const_prefix,
                &z3::Sort::bitvector(context, USIZE_BIT_SIZE),
                &first.z3_sort(),
            );

            for (i, value) in std::iter::once(first).chain(values).enumerate() {
                array = array.store(
                    &ast::BV::from_u64(context, i as u64, USIZE_BIT_SIZE),
                    &value.ast(),
                );
            }

            ArrayNode(
                array,
                ArraySort {
                    range: Box::new(element_sort),
                },
            )
        }

        fn translate_single_proj_result(&mut self, single: &SingleProjResult) -> AstNode<'ctx> {
            match single {
                SingleProjResult::Transmuted(TransmutedValue { value, .. }) => {
                    // NOTE: As transmutation doesn't change the value, we can just translate the value.
                    self.translate_value(value)
                }
                SingleProjResult::Value(value) => self.translate_value(value),
            }
        }

        fn translate_symbolic_proj_result(
            &mut self,
            read_result: &SymbolicProjResult,
            const_prefix: Option<&str>,
        ) -> AstNode<'ctx> {
            match read_result {
                SymbolicProjResult::Single(single) => self.translate_single_proj_result(single),
                SymbolicProjResult::Array(values) => {
                    let const_prefix = const_prefix.unwrap_or(POSSIBLE_VALUES_PREFIX);
                    self.translate_array_of_values(const_prefix, values.iter(), |this, v| {
                        this.translate_symbolic_proj_result(v, Some(const_prefix))
                    })
                    .into()
                }
                SymbolicProjResult::SymRead(select) => self.translate_select(select, const_prefix),
            }
        }

        fn translate_field_on_checked_binary_expression(
            &mut self,
            operator: BinaryOp,
            operands: &SymBinaryOperands,
            field_index: FieldIndex,
        ) -> AstNode<'ctx> {
            const RESULT: u32 = 0;
            const DID_OVERFLOW: u32 = 1;
            match field_index {
                RESULT => {
                    // If we see a `.0` field projection on a symbolic expression that is a checked
                    // binop, we can safely ignore the projection and treat the expression as normal,
                    // since checked binary operations return the tuple `(binop(x, y), did_overflow)`,
                    // and failed checked binops immediately assert!(no_overflow == true), then panic.
                    let unchecked_host = SymValue::Expression(Expr::Binary(BinaryExpr {
                        operator,
                        operands: operands.clone(),
                        checked: false,
                    }));
                    self.translate_symbolic(&unchecked_host)
                }
                DID_OVERFLOW => self.translate_overflow(operator, operands),
                _ => unreachable!("Invalid field index. A checked binop returns a size 2 tuple"),
            }
        }

        /// generate an expression that evaluates true if overflow, false otherwise
        fn translate_overflow(
            &mut self,
            operator: BinaryOp,
            operands: &SymBinaryOperands,
        ) -> AstNode<'ctx> {
            let (left, right) = self.translate_binary_operands(operands);
            let AstNode::BitVector(BVNode(_, BVSort { is_signed })) = left else {
                unreachable!("Overflow only applies to numerical arithmetic operations.")
            };

            let left = left.as_bit_vector();
            let right = right.as_bit_vector();
            let no_overflow = if is_signed {
                let (overflow, underflow) = match operator {
                    BinaryOp::Add => (
                        ast::BV::bvadd_no_overflow(left, right, true),
                        ast::BV::bvadd_no_underflow(left, right),
                    ),
                    BinaryOp::Sub => (
                        ast::BV::bvsub_no_overflow(left, right),
                        ast::BV::bvsub_no_underflow(left, right, true),
                    ),
                    BinaryOp::Mul => (
                        ast::BV::bvmul_no_overflow(left, right, true),
                        ast::BV::bvmul_no_underflow(left, right),
                    ),
                    _ => unreachable!(),
                };
                ast::Bool::and(overflow.get_ctx(), &[&overflow, &underflow])
            } else {
                match operator {
                    BinaryOp::Add => {
                        // note: in unsigned addition, underflow is impossible because there
                        //       are no negative numbers. 0 + 0 is the smallest expression
                        ast::BV::bvadd_no_overflow(left, right, false)
                    }
                    BinaryOp::Sub => {
                        // note: in unsigned subtraction, overflow is impossible because there
                        //       are no negative numbers. max - 0 is the largest expression
                        ast::BV::bvsub_no_underflow(left, right, false)
                    }
                    BinaryOp::Mul => {
                        // note: in unsigned multiplication, underflow is impossible because there
                        //       are no negative numbers. x * 0 is the smallest expression
                        ast::BV::bvmul_no_overflow(left, right, false)
                    }
                    _ => unreachable!(),
                }
            };
            ast::Bool::not(&no_overflow).into()
        }

        fn resolve_proj_expression(&mut self, proj: &ProjExpr) -> SymbolicProjResult {
            ProjExprResolver::resolve(&mut DefaultProjExprReadResolver, proj)
        }
    }

    impl<'ctx> From<AstNode<'ctx>> for ValueRef {
        fn from(ast: AstNode<'ctx>) -> Self {
            match ast {
                AstNode::Bool(ast) => super::super::ConstValue::Bool(ast.as_bool().unwrap()),
                AstNode::BitVector(BVNode(ast, BVSort { is_signed })) => {
                    // TODO: Add support for up to 128-bit integers.
                    let value = if is_signed {
                        let bytes = ast.as_i64().unwrap().to_be_bytes();
                        let mut extended = [0_u8; 16];
                        extended[8..].copy_from_slice(&bytes);
                        u128::from_be_bytes(extended)
                    } else {
                        ast.as_u64().unwrap() as u128
                    };
                    super::super::ConstValue::new_int(
                        value,
                        IntType {
                            bit_size: ast.get_size() as u64,
                            is_signed,
                        },
                    )
                }
                AstNode::Array(_) => {
                    unimplemented!("Symbolic arrays are not supported by this converter.")
                }
            }
            .to_value_ref()
        }
    }
}
