fn main() {
    let f = |x: i32| -> i32 { x + 1 };
    f(10);
    call(f, 10);
}

#[inline(never)]
fn call<I, O>(f: impl Fn(I) -> O, input: I) -> O {
    f(input)
}
