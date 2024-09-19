#![feature(core_intrinsics)]

use leaf::annotations::Symbolizable;

fn main() {
    let a = [1, 2, 3];
    let b = [4, 5];
    let array: [&[i32]; 2] = [&a, &b];
    let x = 1.mark_symbolic();
    // The sample does not generate the theoretical case. Gets optimized.
    let meta = core::intrinsics::ptr_metadata(&*array[x]);
    if meta > 2 {
        foo(());
    } else {
        bar(());
    };
}

#[inline(never)]
fn foo<T>(_x: T) {}

#[inline(never)]
fn bar<T>(_x: T) {}
