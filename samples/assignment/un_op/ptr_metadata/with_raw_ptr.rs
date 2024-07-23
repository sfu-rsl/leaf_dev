#![feature(core_intrinsics)]
fn main() {
    let x = 10;
    let s: *const [i32] = core::intrinsics::aggregate_raw_ptr::<*const [i32], _, _>(&x, 1);
    let meta = core::intrinsics::ptr_metadata(s);
    foo(meta);
}

fn foo<T>(_x: T) {}
