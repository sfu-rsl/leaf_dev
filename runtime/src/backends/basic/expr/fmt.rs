use std::fmt::{Display, Formatter, Result};

use crate::backends::basic::logger::comma_separated;

use super::{sym_place::SymReadResult, *};

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Value::Concrete(value) => write!(f, "{value}"),
            Value::Symbolic(value) => write!(f, "{value}"),
        }
    }
}

impl Display for ConcreteValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ConcreteValue::Const(value) => write!(f, "{value}"),
            ConcreteValue::Adt(value) => write!(f, "{value}"),
            ConcreteValue::Array(value) => write!(f, "{value}"),
            ConcreteValue::Ref(value) => write!(f, "{value}"),
        }
    }
}

impl Display for ConstValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ConstValue::Bool(value) => write!(f, "{value}"),
            ConstValue::Char(value) => write!(f, "'{value}'"),
            ConstValue::Int {
                bit_rep,
                ty: ty @ IntType { is_signed, .. },
            } => {
                if *is_signed {
                    write!(f, "{}", bit_rep.0 as i128)
                } else {
                    write!(f, "{}", bit_rep.0)
                }?;
                write!(f, "{ty}")
            }
            ConstValue::Float { .. } => write!(f, "{self:?}"),
            ConstValue::Str(value) => write!(f, "\"{value}\""),
            ConstValue::Func(_) => write!(f, "{self:?}"),
            ConstValue::Zst => write!(f, "_ZST_"),
        }
    }
}

impl Display for AdtValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.kind {
            AdtKind::Tuple => write!(f, "("),
            AdtKind::Struct => write!(f, "{{"),
            AdtKind::Enum { variant } => write!(f, "V#{}{{", variant),
        }?;
        self.fmt_fields(f)?;
        match self.kind {
            AdtKind::Tuple => write!(f, ")"),
            AdtKind::Struct | AdtKind::Enum { .. } => write!(f, "}}"),
        }?;
        Ok(())
    }
}
impl AdtValue {
    fn fmt_fields(&self, f: &mut Formatter<'_>) -> Result {
        for (index, value) in self.fields.iter().enumerate() {
            write!(f, "{}: ", index)?;
            match value {
                Some(value) => write!(f, "{}", value)?,
                None => write!(f, "_")?,
            };
            write!(f, ", ")?;
        }
        Ok(())
    }
}

impl Display for ArrayValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "[")?;
        for value in self.elements.iter() {
            write!(f, "{}, ", value)?;
        }
        write!(f, "]")
    }
}

impl Display for RefValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            RefValue::Immut(value) => write!(f, "&{value}"),
            RefValue::Mut(full_place) => {
                write!(
                    f,
                    "&mut ({} in {})",
                    AsRef::<crate::backends::basic::Place>::as_ref(full_place),
                    full_place.state_id()
                )
            }
        }
    }
}

impl Display for SymValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SymValue::Variable(var) => write!(f, "{var}"),
            SymValue::Expression(expr) => write!(f, "{expr}"),
        }
    }
}

impl Display for SymbolicVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "<Var{}: {}>", self.id, self.ty)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        // Prefix style
        self.fmt_operator(f)?;
        write!(f, "(")?;
        self.fmt_params(f)?;
        write!(f, ")")?;
        Ok(())
    }
}
impl Expr {
    fn fmt_operator(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expr::Unary { operator, .. } => write!(f, "{operator}"),
            Expr::Binary {
                operator,
                checked: false,
                ..
            } => write!(f, "{operator}"),
            Expr::Binary {
                operator,
                checked: true,
                ..
            } => write!(f, "{operator}ꟲ"),
            Expr::Cast { .. } => write!(f, "Cast"),
            Expr::AddrOf() => write!(f, "AddrOf"),
            Expr::Len(_) => write!(f, "Len"),
            Expr::Projection(_) => write!(f, "Proj"),
        }
    }

    fn fmt_params(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expr::Unary { operand, .. } => write!(f, "{operand}"),
            Expr::Binary { operands, .. } => match operands {
                BinaryOperands::Orig { first, second } => write!(f, "{first}, {second}"),
                BinaryOperands::Rev { first, second } => write!(f, "{first}, {second}"),
            },
            Expr::Cast { from, to } => write!(f, "{from} -> {to}"),
            Expr::AddrOf() => todo!(),
            Expr::Len(of) => write!(f, "{of}"),
            Expr::Projection(proj) => write!(f, "{proj}"),
        }
    }
}

impl Display for ProjExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ProjExpr::SymIndex(ConcreteHostProj {
                host,
                index: SliceIndex { index, from_end },
            }) => write!(f, "({host})[{index}{}]", end_symbol(from_end)),
            ProjExpr::SymHost(SymHostProj { host, kind }) => {
                kind.fmt_pre(f)?;
                write!(f, "({host})")?;
                kind.fmt_post(f)
            }
        }
    }
}

impl ProjKind {
    // FIXME: Almost duplicate with the logger backend's display for Projection

    fn fmt_pre(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ProjKind::Deref => write!(f, "*"),
            _ => Result::Ok(()),
        }
    }

    fn fmt_post(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ProjKind::Deref => Ok(()),
            ProjKind::Field(index) => write!(f, ".{}", index),
            ProjKind::Index(SliceIndex { index, from_end }) => {
                write!(f, "[{index}{}]", end_symbol(from_end))
            }
            ProjKind::Subslice { from, to, from_end } => {
                write!(f, "[{from}..{to}{}]", end_symbol(from_end))
            }
            ProjKind::Downcast(variant) => write!(f, "as V#{}", variant),
        }
    }
}

#[inline]
fn end_symbol(from_end: &bool) -> &str {
    if *from_end { "^" } else { "" }
}

impl Display for SliceIndex<SymValueRef> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}ˢ", self.index, if self.from_end { "^" } else { "" })
    }
}

impl Display for SymReadResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymReadResult::Value(value) => write!(f, "{}", value),
            SymReadResult::Array(values) => write!(f, "{}", comma_separated(values.iter())),
            SymReadResult::SymRead(select) => write!(f, "{}", select),
        }
    }
}
