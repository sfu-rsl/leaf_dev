#![feature(local_key_cell_methods)]
#![feature(associated_type_defaults)]
#![feature(box_patterns)]
#![feature(assert_matches)]
#![feature(iterator_try_collect)]
#![feature(macro_metavar_expr)]
#![feature(result_flattening)]
#![feature(unboxed_closures)]
#![feature(fn_traits)]
#![feature(result_option_inspect)]
#![feature(const_float_bits_conv)]

pub mod abs;
pub mod annotations;
mod backends;
pub(crate) mod outgen;
pub(crate) mod pathics;
pub mod pri;
pub(crate) mod solvers;
pub(crate) mod trace;
pub(crate) mod utils;
