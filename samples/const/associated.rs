trait Num {
    const ZERO: Self;
}

#[allow(dead_code)]
struct U32(u32);
impl Num for U32 {
    const ZERO: Self = U32(0);
}

struct Zero;
impl Num for Zero {
    const ZERO: Self = Zero;
}

fn main() {
    core::hint::black_box(get::<U32>());
    core::hint::black_box(get::<Zero>());
}

#[inline(never)]
fn get<T: Num>() -> T {
    T::ZERO
}
