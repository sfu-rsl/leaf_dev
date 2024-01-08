#![feature(fn_traits)]

fn main() {
    let f = |x: i32, y: i32| -> i32 { x + y };
    call(f, (10, 20));
    call_trait(f, (10, 20));
}

#[inline(never)]
fn call<I1, I2, O>(f: impl Fn(I1, I2) -> O, inputs: (I1, I2)) -> O {
    f(inputs.0, inputs.1)
}

#[inline(never)]
fn call_trait<I1, I2, O>(f: impl Fn(I1, I2) -> O, inputs: (I1, I2)) -> O {
    f.call(inputs)
}
