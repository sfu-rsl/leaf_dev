#![no_std]

pub type LocalIndex = u32;
pub type BasicBlockIndex = u32;
pub type VariantIndex = u32;
pub type FieldIndex = u32;

pub type RawPointer = u64;
pub type PointerOffset = RawPointer;
pub type TypeSize = PointerOffset;
pub type Alignment = TypeSize;

pub type TypeId = u128;

pub type FuncId = RawPointer;

pub mod pri {
    pub type Ref = u64;
    pub type PlaceRef = Ref;
    pub type OperandRef = Ref;

    #[repr(transparent)]
    pub struct BinaryOp(pub u8);
    #[repr(transparent)]
    pub struct UnaryOp(pub u8);

    impl BinaryOp {
        pub const ADD: u8 = 1;
        pub const SUB: u8 = 2;
        pub const MUL: u8 = 3;
        pub const DIV: u8 = 4;
        pub const REM: u8 = 5;
        pub const BIT_XOR: u8 = 6;
        pub const BIT_AND: u8 = 7;
        pub const BIT_OR: u8 = 8;
        pub const SHL: u8 = 9;
        pub const SHR: u8 = 10;
        pub const EQ: u8 = 11;
        pub const LT: u8 = 12;
        pub const LE: u8 = 13;
        pub const NE: u8 = 14;
        pub const GE: u8 = 15;
        pub const GT: u8 = 16;
        pub const OFFSET: u8 = 17;
    }

    impl UnaryOp {
        pub const NOT: u8 = 31;
        pub const NEG: u8 = 32;
    }
}
