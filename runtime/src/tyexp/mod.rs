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
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeVariant {
    // Variant name
    name: String,
    /// Array of types of variant's fields (either def id for AdtTy or name for other Tys).
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
        let type_infos: Vec<TypeInformation> = serde_json::from_str(&content).unwrap_or(Vec::new());
        log::debug!("Reading {:#?} from types.json", type_infos);

        let mut map = HashMap::new();
        for type_info in type_infos.into_iter() {
            map.insert(type_info.def_id, type_info);
        }
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

        file.write_all(
            serde_json::to_string_pretty(&map.values().cloned().collect::<Vec<_>>())
                .unwrap()
                .as_bytes(),
        )
        .unwrap();
    }
}

lazy_static! {
    static ref TYPE_INFO_MAP: Mutex<HashMap<u64, TypeInformation>> = Mutex::new(TypeExport::read());
}

pub(crate) fn get_type_info(def_id: u64) -> Option<TypeInformation> {
    TYPE_INFO_MAP.lock().unwrap().get(&def_id).cloned()
}
