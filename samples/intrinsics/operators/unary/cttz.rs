#![feature(core_intrinsics)]

use core::intrinsics;

use leaf::annotations::Symbolizable;

fn main() {
    let a = 20u8.mark_symbolic();
    let b = unsafe { core::intrinsics::cttz(a) };
    if b == 5 {
        use_num(0);
    }
    let c = unsafe { core::intrinsics::cttz_nonzero(a) };
    if c == 6 {
        use_num(0);
    }
}

fn use_num<T: Default + Eq>(x: T) {
    if x.eq(&T::default()) {
        intrinsics::black_box(x);
    }
}
