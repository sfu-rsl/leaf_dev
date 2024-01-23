use std::mem::transmute;

use leaf::annotations::Symbolizable;

fn main() {
    let x = 10u32.mark_symbolic();
    let y = unsafe { transmute::<u32, [u8; 4]>(x) };
    let z = 0_usize.mark_symbolic();
    if y[z] == 10 && y[z + 1] == 11 {
        foo(x);
    }
}

#[inline(never)]
fn foo<T>(x: T) {}
