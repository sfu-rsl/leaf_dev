use std::mem::transmute;

use runtime::annotations::Symbolizable;

fn main() {
    let array = [10u32, 11, 12, 13];
    let x = 1.mark_symbolic();
    let y = array[x];
    let z = unsafe { transmute::<u32, [u8; 4]>(y) };
    if z[0] == 10 {
        foo(x);
    }
}

#[inline(never)]
fn foo<T>(x: T) {}
