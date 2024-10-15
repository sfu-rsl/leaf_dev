pub(crate) mod chained;
pub(crate) mod composite;
pub(crate) mod logger;
pub(crate) mod macros;
pub(crate) mod proj;
pub(crate) mod sym_place;
pub(crate) mod variance;

use self::macros::macro_rules_method_with_optional_args;

use super::{BinaryOp, CastKind, FloatType, IntType, TypeId, UnaryOp};

macro_rules_method_with_optional_args! (bin_fn_signature {
    ($method: ident + $($arg: ident : $arg_type: ty),* $(,)?) => {
        fn $method<'a>(
            &mut self,
            operands: Self::ExprRefPair<'a>,
            $($arg: $arg_type),*
        ) -> Self::Expr<'a>;
    };
});

macro_rules_method_with_optional_args! (unary_fn_signature {
    ($method: ident + $($arg: ident : $arg_type: ty),* $(,)?) => {
        fn $method<'a>(
            &mut self,
            operand: Self::ExprRef<'a>,
            $($arg: $arg_type),*
        ) -> Self::Expr<'a>;
    };
});

macro_rules_method_with_optional_args! (cast_fn_signature {
    ($method: ident + $($arg: ident : $arg_type: ty),* $(,)?) => {
        fn $method<'a, 'b>(
            &mut self,
            operand: Self::ExprRef<'a>,
            $($arg: $arg_type,)*
            metadata: Self::Metadata<'b>,
        ) -> Self::Expr<'a>;
    };
});

pub(crate) trait BinaryExprBuilder {
    type ExprRefPair<'a>;
    type Expr<'a>;

    bin_fn_signature!(binary_op + op: BinaryOp);

    bin_fn_signature!(add add_unchecked add_with_overflow add_saturating);
    bin_fn_signature!(sub sub_unchecked sub_with_overflow sub_saturating);
    bin_fn_signature!(mul mul_unchecked mul_with_overflow);
    bin_fn_signature!(div rem div_exact);
    bin_fn_signature!(and or xor);
    bin_fn_signature!(shl shl_unchecked shr shr_unchecked);
    bin_fn_signature!(rotate_left rotate_right);
    bin_fn_signature!(eq ne lt le gt ge cmp);
    bin_fn_signature!(offset);
}

pub(crate) trait UnaryExprBuilder {
    type ExprRef<'a>;
    type Expr<'a> = Self::ExprRef<'a>;

    unary_fn_signature!(unary_op + op: UnaryOp);

    unary_fn_signature!(not neg ptr_metadata);
    unary_fn_signature!(bit_reverse);
    unary_fn_signature!(trailing_zeros + non_zero: bool);
}

pub(crate) trait CastExprBuilder {
    type ExprRef<'a>;
    type Expr<'a> = Self::ExprRef<'a>;
    type Metadata<'a> = ();

    type IntType = IntType;
    type FloatType = FloatType;
    type PtrType = TypeId;
    type GenericType = TypeId;

    cast_fn_signature!(
        cast + target: CastKind<Self::IntType, Self::FloatType, Self::PtrType, Self::GenericType>
    );

    cast_fn_signature!(to_char);
    cast_fn_signature!(to_int + ty: Self::IntType);
    cast_fn_signature!(to_float + ty: Self::FloatType);
    cast_fn_signature!(to_ptr + ty: Self::PtrType);
    cast_fn_signature!(ptr_unsize expose_prov sized_dyn);
    cast_fn_signature!(transmute + ty: Self::GenericType);
}

pub(crate) use {
    chained::ChainedExprBuilder, composite::CompositeExprBuilder, logger::LoggerExprBuilder,
};
