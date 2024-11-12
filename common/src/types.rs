use core::num::NonZero;

pub type LocalIndex = u32;
pub type BasicBlockIndex = u32;
pub type VariantIndex = u32;
pub type FieldIndex = u32;

pub type RawAddress = *const ();
pub type PointerOffset = u64;
pub type TypeSize = PointerOffset;
pub type Alignment = TypeSize;

pub type TypeId = NonZero<u128>;

trait SampleTrait {}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct DynRawMetadata(
    // NOTE: Although we know this is a pointer to a vtable, we try to keep it generic.
    pub [u8; core::mem::size_of::<core::ptr::DynMetadata<dyn SampleTrait>>()],
);
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl DynRawMetadata {
    #[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
    #[cfg_attr(core_build, rustc_const_stable(feature = "rust1", since = "1.0.0"))]
    #[inline(always)]
    pub const fn default() -> Self {
        Self([0; core::mem::size_of::<Self>()])
    }
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl Default for DynRawMetadata {
    #[inline(always)]
    fn default() -> Self {
        Self::default()
    }
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl core::fmt::Debug for DynRawMetadata {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Dyn(@{:p})", usize::from_ne_bytes(self.0) as RawAddress)
    }
}

// FIXME: Possibly large data structure.
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct CalleeDef {
    pub static_addr: RawAddress,
    pub as_virtual: Option<(DynRawMetadata, u64)>,
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct FuncDef {
    pub static_addr: RawAddress,
    pub as_dyn_method: Option<(DynRawMetadata, u64)>,
}
