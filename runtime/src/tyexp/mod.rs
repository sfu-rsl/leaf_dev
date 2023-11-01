use std::{
    collections::HashMap,
    fs::OpenOptions,
    io::{Read, Write},
};

use serde::{Deserialize, Serialize};

use crate::abs::{backend::TypeManager, TypeId};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeInformation {
    // A DefId identifies a particular definition, by combining a crate index and a def index.
    def_id: String,
    // Type name.
    name: String,
    // Variants of the ADT. If this is a struct or union, then there will be a single variant.
    variants: Vec<TypeVariant>,
}

impl TypeInformation {
    pub fn new(def_id: String, name: String, variants: Vec<TypeVariant>) -> TypeInformation {
        TypeInformation {
            def_id,
            name,
            variants,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeVariant {
    // Variant name
    name: String,
    /// Array of types of variant's fields.
    /// For AdtTy type field, its DefId (e.g. 0_123) will be put in the array.
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
    pub fn read() -> HashMap<String, TypeInformation> {
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

    pub fn write(map: HashMap<String, TypeInformation>) {
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

pub(crate) struct BasicTypeManager {
    type_map: HashMap<TypeId, TypeInformation>,
}

impl BasicTypeManager {
    pub fn new() -> Self {
        BasicTypeManager {
            type_map: HashMap::new()
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
        self.type_map.insert(key, value.expect("Invalid TypeInformation value"));
    }
}
