use common::tyexp::*;

pub(crate) trait TypeInfoExt {
    fn expect_single_variant(&self) -> &VariantInfo;
    fn child_type_ids(&self) -> impl Iterator<Item = TypeId> + '_;
}

impl TypeInfoExt for TypeInfo {
    fn expect_single_variant(&self) -> &VariantInfo {
        match self.variants.as_slice() {
            [v] => v,
            _ => panic!(
                "Expected the type to have a single variant found {:?}",
                self
            ),
        }
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
