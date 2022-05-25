fn main() {
    let ptr: *const i32 = &0;
    unsafe { *ptr };
}