use leaf::annotations::Symbolizable;

use core::sync::atomic::{AtomicU16, Ordering};

fn main() {
    let a = AtomicU16::new(20u16.mark_symbolic());
    let val = 30u16;
    let previous = a.swap(val, Ordering::SeqCst);
    let current = a.load(Ordering::SeqCst);
    if current == 30u16 {
        core::hint::black_box(0);
    }
    if previous + 10 == 40u16 {
        core::hint::black_box(0);
    }
}
