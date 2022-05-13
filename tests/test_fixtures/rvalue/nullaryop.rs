#[repr(C)]
struct FieldStruct {
    first: u8,
    second: u16,
    third: u8
}

fn main() {
    // FIXME: This doesn't work. NullaryOp apparently only has sizeof and alignof, but these don't
    //  seem to work
    let x = std::mem::align_of::<FieldStruct>();
    let y = x + 1;
}