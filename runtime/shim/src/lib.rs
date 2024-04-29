#![cfg_attr(not(core_build), feature(const_float_bits_conv))]
#![cfg_attr(not(core_build), feature(concat_idents))]
#![cfg_attr(not(core_build), feature(core_intrinsics))]
#![cfg_attr(not(core_build), no_std)]
// -----
#![cfg_attr(core_build, allow(missing_docs))]
#![cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub mod annotations;

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub mod pri;

use common;
