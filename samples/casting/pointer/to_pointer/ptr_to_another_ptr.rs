fn main() {
    let x = 10;
    let y = &x as *const i32;
    let z = y as *const u32;
    foo(z);
}

#[inline(never)]
fn foo<T>(x: T) {}
