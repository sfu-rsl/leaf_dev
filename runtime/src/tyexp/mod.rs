use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fs::OpenOptions,
    io::{Read, Write},
    sync::Mutex,
};

use crate::abs::{backend::TypeManager, TypeId};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeInformation {
    // A DefId identifies a particular definition, by combining a crate index and a def index.
    def_id: u64,
    // Type name.
    name: String,
    // Variants of the ADT. If this is a struct or union, then there will be a single variant.
    variants: Vec<TypeVariant>,
}

impl TypeInformation {
    pub fn new(def_id: u64, name: String, variants: Vec<TypeVariant>) -> TypeInformation {
        TypeInformation {
            def_id,
            name,
            variants,
        }
    }

    pub fn compute_def_id(krate_id: u32, index_id: u32) -> u64 {
        return ((krate_id as u64) << 32) + index_id as u64;
    }

    pub fn revert_def_id(def_id: u64) -> (u32, u32) {
        let krate_id = (def_id >> 32) as u32;
        let index_id = def_id as u32;
        (krate_id, index_id)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeVariant {
    // Variant name
    name: String,
    /// Array of types of variant's fields.
    /// For AdtTy type field, its DefId (e.g. 123) will be put in the array.
    /// For other type fields, its type name (e.g. char) will be put in the array.
    ///
    /// TODO: add a well-defined information structure for other type fields
    fields: Vec<String>,
}

impl TypeVariant {
    pub fn new(name: String, fields: Vec<String>) -> TypeVariant {
        TypeVariant { name, fields }
    }
}

pub struct TypeExport {}

impl TypeExport {
    pub fn read() -> HashMap<u64, TypeInformation> {
        let mut content = String::new();
        let mut file = OpenOptions::new()
            .read(true)
            .open("types.json")
            .expect("Unable to open file for type export");
        file.read_to_string(&mut content).unwrap();
        let map = serde_json::from_str(&content).unwrap_or(HashMap::new());
        log::debug!("Reading {:#?} from types.json", map);
        map
    }

    pub fn write(map: HashMap<u64, TypeInformation>) {
        log::debug!("Writing {:#?} to types.json", map);
        let mut file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open("types.json")
            .expect("Unable to open file for type export");

        file.write_all(serde_json::to_string_pretty(&map).unwrap().as_bytes())
            .unwrap();
    }
}

lazy_static! {
    static ref TYPE_INFO_MAP: Mutex<HashMap<u64, TypeInformation>> = Mutex::new(TypeExport::read());
}

pub(crate) fn get_type_info(def_id: u64) -> Option<TypeInformation> {
    TYPE_INFO_MAP.lock().unwrap().get(&def_id).cloned()
}

pub(crate) struct BasicTypeManager {
    type_map: HashMap<TypeId, TypeInformation>,
}

impl BasicTypeManager {
    pub fn new() -> Self {
        BasicTypeManager {
            type_map: HashMap::new(),
        }
    }
}

impl TypeManager for BasicTypeManager {
    type Key = TypeId;
    type Value = Option<TypeInformation>;

    fn get_type(&self, key: Self::Key) -> Self::Value {
        self.type_map.get(&key).cloned()
    }

    fn set_type(&mut self, key: Self::Key, value: Self::Value) {
        self.type_map
            .insert(key, value.expect("Invalid TypeInformation value"));
    }
}
