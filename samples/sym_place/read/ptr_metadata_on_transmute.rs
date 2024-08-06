#![feature(core_intrinsics)]

use leaf::annotations::Symbolizable;

fn main() {
    let x = 1u128.mark_symbolic();
    let y = unsafe { core::mem::transmute::<u128, *const [u8]>(x) };
    let meta = core::intrinsics::ptr_metadata(y);
    if meta > 1 {
        foo(meta);
    }
}

#[inline(never)]
fn foo<T>(_x: T) {}
