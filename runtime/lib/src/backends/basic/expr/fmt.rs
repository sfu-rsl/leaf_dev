use std::fmt::{Display, Formatter, Result};

use crate::utils::logging::comma_separated;

use super::{
    sym_place::{SingleProjResult, SymbolicProjResult, TransmutedValue},
    PorterValue, RawConcreteValue, *,
};

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
            ConcreteValue::Pointer(value) => write!(f, "{value}"),
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
            AdtKind::Struct => write!(f, "{{"),
            AdtKind::Enum { variant } => write!(f, "V#{}{{", variant),
        }?;
        self.fmt_fields(f)?;
        match self.kind {
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

impl Display for PtrValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.metadata {
            Some(metadata) => write!(f, "({}, {})", self.address, metadata),
            None => write!(f, "{}", self.address),
        }
    }
}

impl Display for UnevalValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            UnevalValue::Some => write!(f, ".C."),
            UnevalValue::Lazy(raw) => write!(f, "{raw}"),
            UnevalValue::Porter(PorterValue { .. }) => write!(f, "{{.S.}}"),
        }
    }
}

impl Display for RawConcreteValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "@({:x}:{})",
            self.0,
            self.1
                .as_ref()
                .map(|t| format!("{}", t))
                .unwrap_or_else(|| format!("{}", self.2))
        )
    }
}

impl Display for LazyTypeInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            LazyTypeInfo::None => write!(f, "?"),
            LazyTypeInfo::Id(id) => write!(f, "T#{:x}", id),
            LazyTypeInfo::Fetched(ty) => write!(f, "T#{:x}", ty.id),
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
            Expr::Binary(BinaryExpr { operator, .. }) => write!(f, "{operator}"),
            Expr::Extension { .. } => write!(f, "Extend"),
            Expr::Extraction { .. } => write!(f, "Extract"),
            Expr::Ite { .. } => write!(f, "Ite"),
            Expr::Multi(_) => write!(f, "Multi"),
            Expr::Ref(_) => write!(f, "&"),
            Expr::Len(_) => write!(f, "Len"),
            Expr::Projection(_) => write!(f, "Proj"),
        }
    }

    fn fmt_params(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expr::Unary { operand, .. } => write!(f, "{operand}"),
            Expr::Binary(BinaryExpr { operands, .. }) => {
                write!(f, "{}, {}", operands.first(), operands.second())
            }
            Expr::Extension {
                source,
                is_zero_ext,
                bits_to_add,
                ty,
            } => write!(
                f,
                "{source}, {}{bits_to_add} as {ty}",
                if *is_zero_ext { "0" } else { "S" }
            ),
            Expr::Extraction {
                source,
                high,
                low,
                ty,
            } => write!(f, "{source}, {high}=..={low} as {ty}"),
            Expr::Ite {
                condition,
                if_target,
                else_target,
            } => write!(f, "{condition} ? {if_target} : {else_target}"),
            Expr::Multi(select) => write!(f, "{select}"),
            Expr::Ref(operand) => write!(f, "{operand}"),
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
                metadata: _,
            }) => write!(f, "({host})[{index}{}]", end_symbol(from_end)),
            ProjExpr::SymHost(SymHostProj {
                host,
                kind,
                metadata: _,
            }) => {
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

impl Display for FieldAccessKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FieldAccessKind::Index(index) => write!(f, "{}", index),
            FieldAccessKind::PtrMetadata => write!(f, "meta"),
        }
    }
}

impl Display for DowncastKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            DowncastKind::EnumVariant(variant) => write!(f, "V#{}", variant),
            DowncastKind::Transmutation(ty_id) => write!(f, "T#{}", ty_id),
        }
    }
}

#[inline]
fn end_symbol(from_end: &bool) -> &str {
    if *from_end { "^" } else { "" }
}

impl Display for SliceIndex<SymValueRef> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}{}ˢ", self.index, if self.from_end { "^" } else { "" })
    }
}

impl Display for SymbolicProjResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SymbolicProjResult::SymRead(select) => write!(f, "{select}"),
            SymbolicProjResult::Array(values) => write!(f, "[{}]", comma_separated(values.iter())),
            SymbolicProjResult::Single(value) => write!(f, "<{value}>"),
        }
    }
}

impl Display for SingleProjResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SingleProjResult::Transmuted(value) => write!(f, "{value}"),
            SingleProjResult::Value(value) => write!(f, "{value}"),
        }
    }
}

impl Display for TransmutedValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} as T#{}", self.value, self.dst_ty_id)
    }
}

impl Display for super::UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let op: abs::UnaryOp = (*self).into();
        op.fmt(f)
    }
}
