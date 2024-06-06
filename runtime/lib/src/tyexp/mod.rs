use common::tyexp::*;

pub(crate) trait TypeInfoExt {
    fn as_single_variant(&self) -> Option<&VariantInfo>;
    fn expect_single_variant(&self) -> &VariantInfo;
    fn child_type_ids(&self) -> impl Iterator<Item = TypeId> + '_;
}

impl TypeInfoExt for TypeInfo {
    #[inline]
    fn as_single_variant(&self) -> Option<&VariantInfo> {
        match self.variants.as_slice() {
            [v] => Some(v),
            _ => None,
        }
    }

    #[inline]
    fn expect_single_variant(&self) -> &VariantInfo {
        self.as_single_variant().unwrap_or_else(|| {
            panic!(
                "Expected the type to have a single variant found {:?}",
                self
            )
        })
    }

    fn child_type_ids(&self) -> impl Iterator<Item = TypeId> + '_ {
        self.variants.iter().flat_map(|v| match &v.fields {
            FieldsShapeInfo::Array(ArrayShape { item_ty, .. }) => vec![*item_ty],
            FieldsShapeInfo::Struct(StructShape { fields }) => {
                fields.iter().map(|f| f.ty).collect()
            }
            _ => vec![],
        })
    }
}

pub(crate) mod instance {
    use std::{collections::HashMap, sync::OnceLock};

    use super::{TypeId, TypeInfo};

    pub(crate) static PROGRAM_TYPES: OnceLock<HashMap<TypeId, TypeInfo>> = OnceLock::new();
}
