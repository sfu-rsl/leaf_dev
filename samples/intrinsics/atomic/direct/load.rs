#![feature(core_intrinsics)]

use core::intrinsics::*;

use leaf::annotations::Symbolizable;

fn main() {
    macro_rules! call_all_and_test {
        ($($op:expr),*$(,)?) => {
            $(
                let mut a = 20u16.mark_symbolic();
                let ptr = &mut a as *const u16;
                let b = unsafe { $op(ptr) };
                if b == 30 {
                    core::hint::black_box(0);
                }
            )*
        };
    }

    call_all_and_test!(
        atomic_load_acquire,
        atomic_load_relaxed,
        atomic_load_seqcst,
        atomic_load_unordered,
    );
}
