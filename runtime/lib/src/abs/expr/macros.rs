macro_rules! repeat_macro_for {
    ($macro: ident; $($item: tt)*) => {
        $(
            $macro!($item);
        )*
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
            match op {
                _ => {
                    let binop = match op {
                        Add => Self::add,
                        AddUnchecked => Self::add_unchecked,
                        AddWithOverflow => Self::add_with_overflow,
                        Sub => Self::sub,
                        SubUnchecked => Self::sub_unchecked,
                        SubWithOverflow => Self::sub_with_overflow,
                        Mul => Self::mul,
                        MulUnchecked => Self::mul_unchecked,
                        MulWithOverflow => Self::mul_with_overflow,
                        Div => Self::div,
                        Rem => Self::rem,
                        BitXor => Self::xor,
                        BitAnd => Self::and,
                        BitOr => Self::or,
                        Shl => Self::shl,
                        ShlUnchecked => Self::shl_unchecked,
                        Shr => Self::shr,
                        ShrUnchecked => Self::shr_unchecked,
                        Eq => Self::eq,
                        Lt => Self::lt,
                        Le => Self::le,
                        Ne => Self::ne,
                        Ge => Self::ge,
                        Gt => Self::gt,
                        Cmp => Self::cmp,
                        Offset => Self::offset,
                    };
                    binop(self, operands)
                }
            }
        }
    };
}

macro_rules! impl_general_binary_op_for {
    ($($method:ident = $op:expr)*) => {
        $(
            fn $method<'a>(
                &mut self,
                operands: <Self as BinaryExprBuilder>::ExprRefPair<'a>,
            ) -> <Self as BinaryExprBuilder>::Expr<'a> {
                self.binary_op(operands, $op)
            }
        )*
    };
    ($($method:ident = $op:expr)* , $arg: ident : $arg_type: ty) => {
        $(
            fn $method<'a>(
                &mut self,
                operands: <Self as BinaryExprBuilder>::ExprRefPair<'a>,
                $arg: $arg_type,
            ) -> <Self as BinaryExprBuilder>::Expr<'a> {
                self.binary_op(operands, $op, $arg)
            }
        )*
    };
}
macro_rules! impl_singular_binary_ops_through_general {
    () => {
        impl_general_binary_op_for!(
            add = BinaryOp::Add
            add_unchecked = BinaryOp::AddUnchecked
            add_with_overflow = BinaryOp::AddWithOverflow
            sub = BinaryOp::Sub
            sub_unchecked = BinaryOp::SubUnchecked
            sub_with_overflow = BinaryOp::SubWithOverflow
            mul = BinaryOp::Mul
            mul_unchecked = BinaryOp::MulUnchecked
            mul_with_overflow = BinaryOp::MulWithOverflow
            div = BinaryOp::Div
            rem = BinaryOp::Rem
            xor = BinaryOp::BitXor
            and = BinaryOp::BitAnd
            or = BinaryOp::BitOr
            shl = BinaryOp::Shl
            shl_unchecked = BinaryOp::ShlUnchecked
            shr = BinaryOp::Shr
            shr_unchecked = BinaryOp::ShrUnchecked
            eq = BinaryOp::Eq
            lt = BinaryOp::Lt
            le = BinaryOp::Le
            ne = BinaryOp::Ne
            ge = BinaryOp::Ge
            gt = BinaryOp::Gt
            cmp = BinaryOp::Cmp
            offset = BinaryOp::Offset
        );
    };
}

#[allow(unused_macros)]
macro_rules! impl_general_unary_op_through_singulars {
    () => {
        fn unary_op<'a>(&mut self, operand: Self::ExprRef<'a>, op: UnaryOp) -> Self::Expr<'a> {
            use UnaryOp::*;
            (match op {
                Not => Self::not,
                Neg => Self::neg,
                PtrMetadata => Self::ptr_metadata,
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
            (ptr_metadata = UnaryOp::PtrMetadata)
        );
    };
}

/// Takes a macro rule with the input of a single method name and many arguments
/// and extends it with two additional patterns for multiple method names and
/// respectively zero and one extra arguments.
macro_rules! macro_rules_method_with_optional_args {
    ($name:ident { $($rule:tt)* }) => {
        macro_rules! $name {
            ($$($$method:ident)*) => {
                $$($name!($$method +);)*
            };
            ($$($$method:ident)* + $$arg: ident : $$arg_type: ty) => {
                $$($name!($$method + $$arg: $$arg_type,);)*
            };
            $($rule)*
        }
    };
}

#[allow(unused_imports)]
pub(crate) use {
    impl_general_binary_op_for, impl_general_binary_op_through_singulars,
    impl_general_unary_op_through_singulars, impl_singular_binary_ops_through_general,
    impl_singular_unary_op_through_general, impl_singular_unary_ops_through_general,
    macro_rules_method_with_optional_args, repeat_macro_for,
};
