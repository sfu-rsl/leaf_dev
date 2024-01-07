use std::{collections::HashMap, sync::OnceLock};

use super::{TypeId, TypeInfo};

pub(crate) static PROGRAM_TYPES: OnceLock<HashMap<TypeId, TypeInfo>> = OnceLock::new();
