use leaf::annotations::Symbolizable;

use core::sync::atomic::{AtomicU16, Ordering};

fn main() {
    let a = AtomicU16::new(20u16.mark_symbolic());
    let val = 30u16;
    let result = a.compare_exchange(20u16, val, Ordering::SeqCst, Ordering::SeqCst);
    let previous = result.unwrap();
    if previous + 10 == 40u16 {
        panic!();
    }

    let new_val = 40u16.mark_symbolic();
    let result = a.compare_exchange(val + 10, new_val, Ordering::SeqCst, Ordering::SeqCst);
    let current = result.unwrap_err();
    if current + 10 == 50u16 {
        panic!();
    }
}
