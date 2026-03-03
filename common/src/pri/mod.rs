mod types;
pub use types::*;

mod defs;
pub mod refs;
pub mod tags;

#[cfg(not(core_build))]
pub use defs::{
    FfiPri, ProgramRuntimeInterface,
    macros::{self, list_func_decls, pass_func_names_to},
};

#[cfg(core_build)]
#[stable(feature = "rust1", since = "1.0.0")]
pub(crate) use defs::{FfiPri, ProgramRuntimeInterface, macros};
