use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[derive(Hash)]
struct AA {
    a: u32,
    b: u64,
}

// from: https://doc.rust-lang.org/std/hash/index.html
fn main() {
    let aa = AA { a: 0, b: 20 };

    let mut s = DefaultHasher::new();
    aa.hash(&mut s);
    let res: u64 = s.finish();
}
