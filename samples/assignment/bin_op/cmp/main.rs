#![feature(core_intrinsics)]

use leaf::annotations::Symbolizable;

fn main() {
    is_less(1, 2);
    let x = 10.mark_symbolic();
    if is_less(x, 10) {
        foo();
    }
}

fn is_less(x: i32, y: i32) -> bool {
    let result = core::intrinsics::three_way_compare(x, y);
    use core::cmp::Ordering::*;
    match result {
        Less => true,
        Equal => false,
        Greater => false,
    }
}

fn foo() {}
