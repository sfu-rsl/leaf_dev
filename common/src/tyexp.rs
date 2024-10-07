use core::{fmt::Display, ops::RangeInclusive};
use std::{collections::HashMap, error::Error, fs::OpenOptions};

use serde::{Deserialize, Serialize, Serializer};

use crate::{types::*, *};

pub use crate::types::{Alignment, TypeId, TypeSize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeInfo {
    pub id: TypeId,
    // Type name.
    pub name: String,
    // Variants of the ADT. If this is a struct or union, then there will be a single variant.
    pub variants: Vec<VariantInfo>,
    pub tag: Option<TagInfo>,

    pub pointee_ty: Option<TypeId>,

    pub align: Alignment,
    pub size: TypeSize,
}

impl TypeInfo {
    pub const SIZE_UNSIZED: TypeSize = TypeSize::MAX;

    #[inline(always)]
    pub fn is_sized(&self) -> bool {
        self.size != Self::SIZE_UNSIZED
    }

    pub fn get_variant(&self, index: VariantIndex) -> Option<&VariantInfo> {
        // There is no guarantee that the index field is as same as the item's index in the array.
        self.variants.iter().find(|v| v.index == index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct VariantInfo {
    pub index: VariantIndex,
    pub fields: FieldsShapeInfo,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FieldsShapeInfo {
    NoFields,
    Array(ArrayShape),
    Struct(StructShape),
    Union(UnionShape),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArrayShape {
    pub len: u64,
    pub item_ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructShape {
    pub fields: Vec<FieldInfo>,
}

// We use the same struct to avoid redundancy. Offset is not used for unions.
pub type UnionShape = StructShape;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FieldInfo {
    pub ty: TypeId,
    pub offset: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TagInfo {
    pub as_field: FieldInfo,
    pub encoding: TagEncodingInfo,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TagEncodingInfo {
    Direct,
    Niche {
        /// The discriminant value when the variant is not a niche.
        non_niche_value: u128,
        /// The range of values for the discriminant when the variant is a niche.
        // NOTE: As the range check is wrapping, we need the end value.
        niche_value_range: RangeInclusive<u128>,
        /// The value of the tag when the variant is at the start of the niche range.
        tag_value_start: u128,
    },
}

pub struct TypeExport;

#[derive(Debug)]
pub struct ReadError {
    pub message: &'static str,
    pub cause: Option<Box<dyn Error>>,
}

impl Error for ReadError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().and_then(|cause| cause.source())
    }
}

impl Display for ReadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)?;
        if let Some(cause) = &self.cause {
            write!(f, " Cause: {}", cause)?;
        }
        Ok(())
    }
}

pub const FINAL_TYPE_EXPORT_FILE: &str = "types.json";

// FIXME: Move these functions to a more appropriate place.
// FIXME: Make this configurable and injectable.
impl TypeExport {
    pub fn read() -> Result<HashMap<TypeId, TypeInfo>, ReadError> {
        let type_info_file_path =
            crate::utils::search_current_ancestor_dirs_for(FINAL_TYPE_EXPORT_FILE)
                .expect("Failed to find the type info file.");
        let type_infos: Vec<TypeInfo> =
            Self::get_type_info(type_info_file_path.display().to_string())?;
        log_debug!("Retrieved {} types from file.", type_infos.len());

        let type_infos = type_infos
            .into_iter()
            .map(|type_info| (type_info.id, type_info))
            .collect();
        Ok(type_infos)
    }

    pub fn write<'a>(types: impl Iterator<Item = &'a TypeInfo>, file_path: String) {
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(file_path)
            .expect("Unable to open file for type export");
        let mut serializer = serde_json::Serializer::pretty(file);
        serializer
            .collect_seq(types)
            .expect("Failed to write types to file.");
    }

    pub fn get_type_info(file_path: String) -> Result<Vec<TypeInfo>, ReadError> {
        let file = OpenOptions::new()
            .read(true)
            .open(file_path)
            .map_err(|err| ReadError {
                message: "Failed to open file for type export",
                cause: Some(Box::new(err)),
            })?;

        let type_infos: Vec<TypeInfo> = serde_json::from_reader(file).map_err(|err| ReadError {
            message: "Failed to parse types from file.",
            cause: Some(Box::new(err)),
        })?;
        Ok(type_infos)
    }
}
