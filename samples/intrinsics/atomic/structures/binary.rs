use leaf::annotations::Symbolizable;

use core::sync::atomic::{AtomicU8, Ordering};

fn main() {
    let a = AtomicU8::new(20u8.mark_symbolic());
    let original = a.load(Ordering::SeqCst);
    let val = 0b00001010u8;
    let check = 0b00001000u8;
    let previous = a.fetch_add(val, Ordering::SeqCst);
    let current = a.load(Ordering::SeqCst);
    if current == check {
        core::hint::black_box(0);
    }
    if previous == original {
        core::hint::black_box(0);
    }
}
