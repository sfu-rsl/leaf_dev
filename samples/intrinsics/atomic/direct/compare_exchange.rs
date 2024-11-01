#![feature(core_intrinsics)]

use core::intrinsics::*;

use leaf::annotations::Symbolizable;

fn main() {
    macro_rules! call_all_and_test {
        ($($op:expr),*$(,)?) => {
            $(
                let mut a = 10u16;
                let val = 20u16.mark_symbolic();
                let ptr = &mut a as *mut u16;
                let (previous, successful) = unsafe { $op(ptr, 10u16, val) };
                assert!(successful);

                let original = a;
                let val = 30u16;
                let (previous, successful) = unsafe { $op(ptr, original, val) };
                // Symbolic condition
                assert!(successful);
                if previous + 10 == 40u16 {
                    core::hint::black_box(0);
                }

                let new_val = 40u16.mark_symbolic();
                let (previous, successful) = unsafe { $op(ptr, val, new_val) };
                // Semi-symbolic condition
                assert!(successful);
                if previous + 10 == 40u16 {
                    core::hint::black_box(0);
                } else {
                    panic!();
                }

                let (previous, successful) = unsafe { $op(ptr, 50u16, original) };
                // Symbolic condition
                assert!(!successful);
                if previous + 10 == 60u16 {
                    panic!();
                }
            )*
        };
    }

    call_all_and_test!(
        atomic_cxchg_acqrel_acquire,
        atomic_cxchg_acqrel_relaxed,
        atomic_cxchg_acqrel_seqcst,
        atomic_cxchg_acquire_acquire,
        atomic_cxchg_acquire_relaxed,
        atomic_cxchg_acquire_seqcst,
        atomic_cxchg_relaxed_acquire,
        atomic_cxchg_relaxed_relaxed,
        atomic_cxchg_relaxed_seqcst,
        atomic_cxchg_release_acquire,
        atomic_cxchg_release_relaxed,
        atomic_cxchg_release_seqcst,
        atomic_cxchg_seqcst_acquire,
        atomic_cxchg_seqcst_relaxed,
        atomic_cxchg_seqcst_seqcst,
        atomic_cxchgweak_acqrel_acquire,
        atomic_cxchgweak_acqrel_relaxed,
        atomic_cxchgweak_acqrel_seqcst,
        atomic_cxchgweak_acquire_acquire,
        atomic_cxchgweak_acquire_relaxed,
        atomic_cxchgweak_acquire_seqcst,
        atomic_cxchgweak_relaxed_acquire,
        atomic_cxchgweak_relaxed_relaxed,
        atomic_cxchgweak_relaxed_seqcst,
        atomic_cxchgweak_release_acquire,
        atomic_cxchgweak_release_relaxed,
        atomic_cxchgweak_release_seqcst,
        atomic_cxchgweak_seqcst_acquire,
        atomic_cxchgweak_seqcst_relaxed,
        atomic_cxchgweak_seqcst_seqcst,
    );
}
