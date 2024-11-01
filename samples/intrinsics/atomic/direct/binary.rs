#![feature(core_intrinsics)]

use core::intrinsics::*;

use leaf::annotations::Symbolizable;

fn main() {
    macro_rules! call_all_and_test {
        ($($op:expr),*$(,)?) => {
            $(
                let mut a = 20u8.mark_symbolic();
                let original = a;
                let ptr = &mut a as *mut u8;
                let val = 0b00001010u8;
                let check = 0b00001000u8;
                let previous = unsafe { $op(ptr, val) };
                let current = unsafe { *ptr };
                if current == check {
                    core::hint::black_box(0);
                }
                if previous == original {
                    core::hint::black_box(0);
                }
            )*
        };
    }
    call_all_and_test!(
        atomic_and_acqrel,
        atomic_and_acquire,
        atomic_and_relaxed,
        atomic_and_release,
        atomic_and_seqcst,
        // atomic_max_acqrel,
        // atomic_max_acquire,
        // atomic_max_relaxed,
        // atomic_max_release,
        // atomic_max_seqcst,
        // atomic_min_acqrel,
        // atomic_min_acquire,
        // atomic_min_relaxed,
        // atomic_min_release,
        // atomic_min_seqcst,
        // atomic_nand_acqrel,
        // atomic_nand_acquire,
        // atomic_nand_relaxed,
        // atomic_nand_release,
        // atomic_nand_seqcst,
        atomic_or_acqrel,
        atomic_or_acquire,
        atomic_or_relaxed,
        atomic_or_release,
        atomic_or_seqcst,
        // atomic_umax_acqrel,
        // atomic_umax_acquire,
        // atomic_umax_relaxed,
        // atomic_umax_release,
        // atomic_umax_seqcst,
        // atomic_umin_acqrel,
        // atomic_umin_acquire,
        // atomic_umin_relaxed,
        // atomic_umin_release,
        // atomic_umin_seqcst,
        atomic_xadd_acqrel,
        atomic_xadd_acquire,
        atomic_xadd_relaxed,
        atomic_xadd_release,
        atomic_xadd_seqcst,
        atomic_xor_acqrel,
        atomic_xor_acquire,
        atomic_xor_relaxed,
        atomic_xor_release,
        atomic_xor_seqcst,
        atomic_xsub_acqrel,
        atomic_xsub_acquire,
        atomic_xsub_relaxed,
        atomic_xsub_release,
        atomic_xsub_seqcst,
    );
}
