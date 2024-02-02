#![feature(core_intrinsics)]
/* NOTE: Normal programs are not expected to call intrinsics directly.
 * However, this sample can imitate what happens in the core library. */
fn main() {
    let a = get_num(5);
    if core::intrinsics::bitreverse(a) == a {
        foo();
    }
}

fn get_num(x: u8) -> u8 {
    x + 5
}

#[inline(never)]
fn foo() {}
