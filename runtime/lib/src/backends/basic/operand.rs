use crate::abs::{backend::implementation::DefaultOperandHandler, Constant};

use super::expr::SymValueRef;

pub(crate) type Operand<P, S = SymValueRef> = crate::abs::Operand<P, Constant, S>;

pub(crate) type BasicOperandHandler<'a, P, S = SymValueRef> = DefaultOperandHandler<'a, P, S>;
