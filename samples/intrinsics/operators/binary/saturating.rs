#![feature(core_intrinsics)]

use core::intrinsics;

use leaf::annotations::Symbolizable;

fn main() {
    let a = 20u8.mark_symbolic();

    let b = intrinsics::saturating_add(a, 100);
    if b < 100 {
        // This branch is unreachable (no wrapping)
        use_num(b);
    }

    let c = intrinsics::saturating_sub(a, 100);
    if c > u8::MAX - 100 {
        // This branch is unreachable (no wrapping)
        use_num(c);
    }
}

fn use_num<T: Default + Eq>(x: T) {
    if x.eq(&T::default()) {
        intrinsics::black_box(x);
    }
}
