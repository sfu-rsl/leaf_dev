use std::mem::transmute;

use leaf::annotations::Symbolizable;

fn main() {
    let x = 10u32.mark_symbolic();
    let y = read_index(x);
    foo(y);
    let z = read_field(x);
    foo(z);

    // Unsatisfiable:
    if y != z {
        bar();
    }
}

fn foo(x: u8) {
    if x == 1 {
        bar();
    }
}

#[inline(never)]
fn bar() {}

struct Splitter {
    head: u8,
    tail: [u8; 3],
}

fn read_index(x: u32) -> u8 {
    let array = unsafe { transmute::<u32, [u8; 4]>(x) };
    array[0]
}

fn read_field(x: u32) -> u8 {
    let splitter = unsafe { transmute::<u32, Splitter>(x) };
    splitter.head
}
