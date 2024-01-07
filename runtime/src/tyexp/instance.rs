use std::{sync::OnceLock, collections::HashMap};

use super::{TypeInfo, TypeId};

pub(crate) static PROGRAM_TYPES : OnceLock<HashMap<TypeId, TypeInfo>> = OnceLock::new();