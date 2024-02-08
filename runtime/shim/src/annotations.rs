use core::concat_idents;
use core::mem::size_of;

use crate::pri::*;

pub trait Symbolizable: Sized {
    fn symbolize();

    fn mark_symbolic(self) -> Self {
        Self::symbolize();
        self
    }
}

macro_rules! impl_symbolizable_direct {
    ($($ty:ident),*) => {
        $(
            impl Symbolizable for $ty {
                fn symbolize() {
                    let operand_ref = concat_idents!(new_sym_value_, $ty)();
                    override_return_value(operand_ref);
                }
            }
        )*
    };
}
impl_symbolizable_direct!(bool, char);

macro_rules! impl_symbolizable_int {
    ($($ty:ty),*, $signed:literal) => {
        $(
            impl Symbolizable for $ty {
                fn symbolize() {
                    let operand_ref = new_sym_value_int(size_of::<$ty>() as u64 * 8, $signed);
                    override_return_value(operand_ref);
                }
            }
        )*
    };
}

impl_symbolizable_int!(i8, i16, i32, i64, i128, isize, true);
impl_symbolizable_int!(u8, u16, u32, u64, u128, usize, false);

macro_rules! impl_symbolizable_float {
    ($($ty:ty),*) => {
        $(
            impl Symbolizable for $ty {
                fn symbolize() {
                    let sbits = <$ty>::MANTISSA_DIGITS as u64;
                    let ebits = (<$ty>::MAX_EXP - <$ty>::MIN_EXP + 1) as u64;
                    let operand_ref = new_sym_value_float(ebits, sbits);
                    override_return_value(operand_ref);
                }
            }
        )*
    };
}

impl_symbolizable_float!(f32, f64);
