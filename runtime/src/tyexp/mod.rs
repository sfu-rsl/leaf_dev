pub(crate) mod instance;

use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fs::OpenOptions,
    io::{Read, Write},
};

use crate::abs::{Alignment, TypeSize, VariantIndex};

type TypeId = u128;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeInfo {
    pub id: TypeId,
    // Type name.
    pub name: String,
    // Variants of the ADT. If this is a struct or union, then there will be a single variant.
    pub variants: Vec<VariantInfo>,

    pub align: Alignment,
    pub size: TypeSize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariantInfo {
    pub index: VariantIndex,
    pub fields: FieldsShapeInfo,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FieldsShapeInfo {
    NoFields,
    Union,
    Array(ArrayShape),
    Struct(StructShape),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArrayShape {
    pub len: u64,
    pub item_ty: TypeId,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructShape {
    pub fields: Vec<FieldInfo>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldInfo {
    pub ty: TypeId,
    pub offset: u64,
}

impl TypeInfo {
    pub(crate) fn expect_single_variant(&self) -> &VariantInfo {
        match self.variants.as_slice() {
            [v] => v,
            _ => panic!(
                "Expected the type to have a single variant found {:?}",
                self
            ),
        }
    }

    pub fn child_type_ids(&self) -> impl Iterator<Item = TypeId> + '_ {
        self.variants.iter().flat_map(|v| match &v.fields {
            FieldsShapeInfo::Array(ArrayShape { item_ty, .. }) => vec![*item_ty],
            FieldsShapeInfo::Struct(StructShape { fields }) => {
                fields.iter().map(|f| f.ty).collect()
            }
            _ => vec![],
        })
    }
}

pub struct TypeExport;

impl TypeExport {
    pub fn read() -> HashMap<TypeId, TypeInfo> {
        let mut content = String::new();
        let mut file = OpenOptions::new()
            .read(true)
            .open("types.json")
            .expect("Unable to open file for type export");
        file.read_to_string(&mut content).unwrap();
        let type_infos: Vec<TypeInfo> = serde_json::from_str(&content).unwrap_or(Vec::new());

        let mut map = HashMap::new();
        for type_info in type_infos.into_iter() {
            map.insert(type_info.id, type_info);
        }
        map
    }

    pub fn write(map: &HashMap<TypeId, TypeInfo>) {
        let mut file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open("types.json")
            .expect("Unable to open file for type export");

        file.write_all(
            serde_json::to_string_pretty(&map.values().cloned().collect::<Vec<_>>())
                .unwrap()
                .as_bytes(),
        )
        .unwrap();
    }
}
