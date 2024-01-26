use std::{collections::HashMap, fs::OpenOptions};

use serde::{Deserialize, Serialize, Serializer};

use crate::{types::*, *};

pub use crate::types::TypeId;

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

pub struct TypeExport;

// FIXME: Move these functions to a more appropriate place.
// FIXME: Make this configurable and injectable.
impl TypeExport {
    pub fn read() -> HashMap<TypeId, TypeInfo> {
        let file = OpenOptions::new()
            .read(true)
            .open("types.json")
            .expect("Failed to open file for type export");

        let type_infos: Vec<TypeInfo> =
            serde_json::from_reader(file).expect("Failed to parse types from file.");
        log::debug!("Retrieved {} types from file.", type_infos.len());

        type_infos
            .into_iter()
            .map(|type_info| (type_info.id, type_info))
            .collect()
    }

    pub fn write<'a>(types: impl Iterator<Item = &'a TypeInfo>) {
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open("types.json")
            .expect("Unable to open file for type export");

        let mut serializer = serde_json::Serializer::pretty(file);
        serializer
            .collect_seq(types)
            .expect("Failed to write types to file.");
    }
}
