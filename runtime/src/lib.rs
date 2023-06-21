#![feature(local_key_cell_methods)]
#![feature(associated_type_defaults)]
#![feature(box_patterns)]
#![feature(assert_matches)]
#![feature(iterator_try_collect)]
#![feature(result_flattening)]

pub mod abs;
pub mod annotations;
mod backends;
pub(crate) mod outgen;
pub(crate) mod pathics;
pub mod pri;
pub(crate) mod solvers;
pub(crate) mod trace;
pub(crate) mod utils;
