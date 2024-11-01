#![feature(core_intrinsics)]

use core::intrinsics::*;

use leaf::annotations::Symbolizable;

fn main() {
    macro_rules! call_all_and_test {
        ($($op:expr),*$(,)?) => {
            $(
                unsafe { $op(); }
            )*
        };
    }

    call_all_and_test!(
        atomic_fence_acqrel,
        atomic_fence_acquire,
        atomic_fence_release,
        atomic_fence_seqcst,
        atomic_singlethreadfence_acqrel,
        atomic_singlethreadfence_acquire,
        atomic_singlethreadfence_release,
        atomic_singlethreadfence_seqcst,
    );
}
