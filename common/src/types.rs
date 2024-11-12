pub type LocalIndex = u32;
pub type BasicBlockIndex = u32;
pub type VariantIndex = u32;
pub type FieldIndex = u32;

pub type RawAddress = *const ();
pub type PointerOffset = u64;
pub type TypeSize = PointerOffset;
pub type Alignment = TypeSize;

pub type TypeId = core::num::NonZero<u128>;

pub type FuncId = RawPointer;
