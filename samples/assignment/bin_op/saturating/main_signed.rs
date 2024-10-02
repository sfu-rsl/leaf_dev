#![feature(core_intrinsics)]

use core::intrinsics;

use leaf::annotations::Symbolizable;

fn main() {
    let a = 20i8.mark_symbolic();

    let b = a.saturating_add(100);
    if b < i8::MIN + 100 {
        // This branch is unreachable (no wrapping)
        use_num(b);
    }
    let b = a.saturating_add(-100);
    if b > i8::MAX - 100 {
        // This branch is unreachable (no wrapping)
        use_num(b);
    }

    let b = a.saturating_sub(100);
    if b > i8::MAX - 100 {
        // This branch is unreachable (no wrapping)
        use_num(b);
    }
    let b = a.saturating_sub(-100);
    if b < i8::MIN + 100 {
        // This branch is unreachable (no wrapping)
        use_num(b);
    }
}

fn use_num<T: Default + Eq>(x: T) {
    if x.eq(&T::default()) {
        intrinsics::black_box(x);
    }
}
