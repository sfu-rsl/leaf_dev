#![feature(core_intrinsics)]

use leaf::annotations::Symbolizable;

fn main() {
    let a = [1, 2, 3];
    let b = [4, 5];
    let array: [&[i32]; 2] = [&a, &b];
    let x = 1.mark_symbolic();
    let y = array[x];
    foo(y);
    let ptr = y as *const [i32];
    let meta = core::intrinsics::ptr_metadata(ptr);
    foo(meta);
}

#[inline(never)]
fn foo<T>(_x: T) {}
