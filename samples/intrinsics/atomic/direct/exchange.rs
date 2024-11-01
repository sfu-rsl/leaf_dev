#![feature(core_intrinsics)]

use core::intrinsics::*;

use leaf::annotations::Symbolizable;

fn main() {
    macro_rules! call_all_and_test {
        ($($op:expr),*$(,)?) => {
            $(
                let mut a = 20u16.mark_symbolic();
                let original = a;
                let ptr = &mut a as *mut u16;
                let val = 30u16;
                let previous = unsafe { $op(ptr, val) };
                let current = unsafe { *ptr };

                // Concrete condition
                if current == 30u16 && current == a {
                    core::hint::black_box(0);
                } else {
                    panic!();
                }
                // Symbolic condition
                if previous == original  {
                    core::hint::black_box(0);
                }

                let new_val = 40u16.mark_symbolic();
                let previous = unsafe { $op(ptr, new_val) };
                let current = unsafe { *ptr };

                // Symbolic condition
                if (current == 50u16) & (a + 10 == 60u16) {
                    core::hint::black_box(0);
                }

                // Concrete condition
                if previous == val {
                    core::hint::black_box(0);
                } else {
                    panic!();
                }
            )*
        };
    }

    call_all_and_test!(
        atomic_xchg_acqrel,
        atomic_xchg_acquire,
        atomic_xchg_relaxed,
        atomic_xchg_release,
        atomic_xchg_seqcst,
    );
}
