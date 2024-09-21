use std::fmt::{Display, Formatter, Result};

use super::{
    place::{
        DerefSymHostPlace, DeterministicProjection, PlaceValue, SymIndexedPlace, SymbolicPlaceBase,
        SymbolicPlaceValue,
    },
    *,
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
            ConcreteValue::FatPointer(value) => write!(f, "{value}"),
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
            ConstValue::Func(id) => write!(f, "_f@{id:p}"),
            ConstValue::Addr(addr) => write!(f, "ᴬ{addr:p}"),
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

impl Display for FatPtrValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({}, {})", self.address, self.metadata)
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
            "@({:p}:{})",
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
            LazyTypeInfo::Forced(ty) => write!(f, "T#ᶠ{:x}", ty.id),
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

impl Display for super::UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let op: abs::UnaryOp = (*self).into();
        op.fmt(f)
    }
}

impl Display for PlaceValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            PlaceValue::Deterministic(value) => write!(f, "{:?}", value),
            PlaceValue::Symbolic(value) => write!(f, "{value}"),
        }
    }
}

impl Display for SymbolicPlaceValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.base)?;
        if let Some(proj) = &self.proj {
            write!(f, ".{{{}}}", proj)?;
        }
        Ok(())
    }
}
impl Display for SymbolicPlaceBase {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SymbolicPlaceBase::Deref(host) => write!(f, "*{host}"),
            SymbolicPlaceBase::SymIndex(indexed) => write!(f, "{indexed}"),
        }
    }
}

impl Display for DerefSymHostPlace {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.value)
    }
}

impl Display for SymIndexedPlace {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}[{}]", self.host, self.index)
    }
}

impl Display for DeterministicProjection {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "↳{}T#{:x}", self.offset, self.ty_id)
    }
}
