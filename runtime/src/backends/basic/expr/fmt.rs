use std::fmt::{Display, Formatter, Result};

use crate::backends::basic::logger::comma_separated;

use super::{sym_place::SymbolicProjResult, PorterValue, RawConcreteValue, *};

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
            ConcreteValue::Unevaluated(value) => write!(f, "{value}"),
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
            ConstValue::Func(id) => write!(f, "_f@{id:p}"),
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
        for (index, field) in self.fields.iter().enumerate() {
            write!(f, "{}", index)?;
            write!(f, ": ")?;
            match &field.value {
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

impl Display for UnevalValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            UnevalValue::Some => write!(f, ".C."),
            UnevalValue::Lazy(RawConcreteValue(addr, ty)) => write!(
                f,
                "@({:x}:{})",
                addr,
                ty.as_ref().map_or("?".to_owned(), |ty| format!("{}", ty))
            ),
            UnevalValue::Porter(PorterValue { .. }) => write!(f, "{{.S.}}"),
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
            Expr::Binary(BinaryExpr {
                operator,
                checked: false,
                ..
            }) => write!(f, "{operator}"),
            Expr::Binary(BinaryExpr {
                operator,
                checked: true,
                ..
            }) => write!(f, "{operator}ꟲ"),
            Expr::Cast { .. } => write!(f, "Cast"),
            Expr::Extension { .. } => write!(f, "Extend"),
            Expr::Extraction { .. } => write!(f, "Extract"),
            Expr::Ite {
                source,
                first_target,
                second_target,
                ..
            } => write!(f, "Ite {source} {first_target} {second_target}"),
            Expr::AddrOf(_) => write!(f, "AddrOf"),
            Expr::Len(_) => write!(f, "Len"),
            Expr::Projection(_) => write!(f, "Proj"),
        }
    }

    fn fmt_params(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expr::Unary { operand, .. } => write!(f, "{operand}"),
            Expr::Binary(BinaryExpr { operands, .. }) => match operands {
                BinaryOperands::Orig { first, second } => write!(f, "{first}, {second}"),
                BinaryOperands::Rev { first, second } => write!(f, "{first}, {second}"),
            },
            Expr::Cast { from, to } => write!(f, "{from} -> {to}"),
            Expr::Extension {
                source,
                is_zero_ext,
                bits_to_add,
                is_signed,
            } => write!(f, "{source}, {is_zero_ext}, {bits_to_add}, {is_signed}"),
            Expr::Extraction {
                source,
                high,
                low,
                is_signed,
            } => write!(f, "{source}, {high}, {low}, {is_signed}"),
            Expr::Ite {
                source,
                first_target,
                second_target,
                is_signed,
            } => write!(f, "{source}, {first_target}, {second_target}, {is_signed}"),
            Expr::AddrOf(operand) => write!(f, "{operand}"),
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
            ProjKind::Downcast(kind) => write!(f, " as {}", kind),
        }
    }
}

impl Display for DowncastKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            DowncastKind::EnumVariant(variant) => write!(f, "V#{}", variant),
            DowncastKind::Transmutation(ty_id) => write!(f, "T#{:p}", ty_id),
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

impl Display for SymbolicProjResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolicProjResult::SymRead(select) => write!(f, "{}", select),
            SymbolicProjResult::Array(values) => write!(f, "{}", comma_separated(values.iter())),
            SymbolicProjResult::Single(value) => match value {
                sym_place::SingleProjResult::Transmuted(trans) => {
                    write!(f, "{} as T#{}", trans.value, trans.dst_ty_id)
                }
                sym_place::SingleProjResult::Value(value) => write!(f, "{}", value),
            },
        }
    }
}
