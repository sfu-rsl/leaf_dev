fn main() {
    let x = 10;
    let y = &x as *const i32;
    let z = y as *const i64;
    foo(z);
}

#[inline(never)]
fn foo<T>(x: T) {}
