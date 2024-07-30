pub type LocalIndex = u32;
pub type BasicBlockIndex = u32;
pub type VariantIndex = u32;
pub type FieldIndex = u32;

// FIXME: Rename to `RawAddress` because raw pointers in Rust may be fat.
pub type RawPointer = u64;
pub type RawAddress = RawPointer;
pub type PointerOffset = RawPointer;
pub type TypeSize = PointerOffset;
pub type Alignment = TypeSize;

pub type TypeId = core::num::NonZero<u128>;

pub type FuncId = RawPointer;
