use leaf::annotations::Symbolizable;

use core::sync::atomic::{AtomicU16, Ordering};

fn main() {
    let a = AtomicU16::new(0);
    a.store(20u16.mark_symbolic(), Ordering::SeqCst);
    let b = a.load(Ordering::SeqCst);
    if b == 30 {
        core::hint::black_box(0);
    }
}
