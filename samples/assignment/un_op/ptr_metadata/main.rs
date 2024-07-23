#![feature(core_intrinsics)]

fn main() {
    let a = [1, 2, 3];
    let b: &[i32] = &a;
    let ptr = to_ptr(b);
    let meta = core::intrinsics::ptr_metadata(ptr);
    foo(meta);
}

#[inline(never)]
fn to_ptr<T: ?Sized>(x: &T) -> *const T {
    x as *const T
}

#[inline(never)]
fn foo<T>(_x: T) {}
