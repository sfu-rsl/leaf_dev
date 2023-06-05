macro_rules! repeat_macro_for {
    ($macro: ident; $($item: tt)*) => {
        $(
            $macro!($item);
        )*
    };
}

macro_rules! for_all_binary_op {
    ($macro: ident) => {
        repeat_macro_for!(
            $macro; add sub mul div rem and or xor shl shr eq ne lt le gt ge offset
        );
    };
}

macro_rules! impl_general_binary_op_through_singulars {
    () => {
        fn binary_op<'a>(
            &mut self,
            operands: Self::ExprRefPair<'a>,
            op: BinaryOp,
        ) -> Self::Expr<'a> {
            use BinaryOp::*;
            (match op {
                Add => Self::add,
                Sub => Self::sub,
                Mul => Self::mul,
                Div => Self::div,
                Rem => Self::rem,
                BitXor => Self::xor,
                BitAnd => Self::and,
                BitOr => Self::or,
                Shl => Self::shl,
                Shr => Self::shr,
                Eq => Self::eq,
                Lt => Self::lt,
                Le => Self::le,
                Ne => Self::ne,
                Ge => Self::ge,
                Gt => Self::gt,
                Offset => Self::offset,
            })(self, operands)
        }
    };
}

macro_rules! impl_general_binary_op_for_singular {
    (($method:ident = $op:expr)) => {
        fn $method<'a>(
            &mut self,
            operands: <Self as BinaryExprBuilder>::ExprRefPair<'a>,
        ) -> <Self as BinaryExprBuilder>::Expr<'a> {
            self.binary_op(operands, $op)
        }
    };
}
macro_rules! impl_singular_binary_ops_through_general {
    () => {
        repeat_macro_for!(
            impl_general_binary_op_for_singular;
            (add = BinaryOp::Add)
            (sub = BinaryOp::Sub)
            (mul = BinaryOp::Mul)
            (div = BinaryOp::Div)
            (rem = BinaryOp::Rem)
            (xor = BinaryOp::BitXor)
            (and = BinaryOp::BitAnd)
            (or = BinaryOp::BitOr)
            (shl = BinaryOp::Shl)
            (shr = BinaryOp::Shr)
            (eq = BinaryOp::Eq)
            (lt = BinaryOp::Lt)
            (le = BinaryOp::Le)
            (ne = BinaryOp::Ne)
            (ge = BinaryOp::Ge)
            (gt = BinaryOp::Gt)
            (offset = BinaryOp::Offset)
        );
    };
}

#[allow(unused_macros)]
macro_rules! impl_general_unary_op_through_singulars {
    () => {
        fn unary_op<'a>(&mut self, operand: Self::ExprRefPair<'a>, op: UnaryOp) -> Self::Expr<'a> {
            use UnaryOp::*;
            (match op {
                Not => Self::not,
                Neg => Self::neg,
            })(self, operand)
        }
    };
}

macro_rules! impl_singular_unary_op_through_general {
    (($method:ident = $op:expr)) => {
        fn $method<'a>(&mut self, operand: Self::ExprRef<'a>) -> Self::Expr<'a> {
            self.unary_op(operand, $op)
        }
    };
}
macro_rules! impl_singular_unary_ops_through_general {
    () => {
        repeat_macro_for!(
            impl_singular_unary_op_through_general;
            (not = UnaryOp::Not)
            (neg = UnaryOp::Neg)
        );
    };
}

#[allow(unused_imports)]
pub(crate) use {
    for_all_binary_op, impl_general_binary_op_for_singular,
    impl_general_binary_op_through_singulars, impl_general_unary_op_through_singulars,
    impl_singular_binary_ops_through_general, impl_singular_unary_op_through_general,
    impl_singular_unary_ops_through_general, repeat_macro_for,
};
