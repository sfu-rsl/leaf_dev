#![feature(core_intrinsics)]
fn main() {
    let x = 10;
    let s: &[i32] = unsafe { &*core::intrinsics::aggregate_raw_ptr::<*const [i32], _, _>(&x, 1) };
    foo(s);
}

fn foo<T>(_x: T) {}
