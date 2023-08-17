use std::{
    collections::btree_map::Values,
    fmt::{Display, Formatter, Result},
};

use crate::backends::basic::logger::comma_separated;

use super::{
    expr::sym_place::{Select, SelectTarget},
    *,
};

impl<V> Display for Constraint<V>
where
    V: Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Constraint::Bool(value) => write!(f, "({})", value),
            Constraint::Not(value) => write!(f, "!({})", value),
        }
    }
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::Int(int) => write!(f, "{}", int),
            Self::Float(float) => write!(f, "{}", float),
        }
    }
}

impl Display for IntType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "{}{}",
            if self.is_signed { 'i' } else { 'u' },
            self.bit_size
        )
    }
}

impl Display for FloatType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "f{}", self.e_bits + self.s_bits)
    }
}

impl Display for Local {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::ReturnValue => write!(f, "ReturnValue"),
            Self::Argument(local) => write!(f, "Arg({})", local),
            Self::Normal(local) => write!(f, "Var({})", local),
        }
    }
}

impl<I, V> Display for Select<I, V>
where
    I: Display,
    V: Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}[{}]", self.target, self.index)
    }
}

impl<V, S> Display for SelectTarget<V, S>
where
    V: Display,
    S: Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            SelectTarget::Array(values) => write!(f, "{}", comma_separated(values.iter())),
            SelectTarget::Nested(box select) => write!(f, "{select}"),
        }
    }
}
