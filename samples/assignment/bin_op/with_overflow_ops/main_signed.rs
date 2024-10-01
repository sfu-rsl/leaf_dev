use leaf::annotations::Symbolizable;

fn main() {
    let x = 20_i8.mark_symbolic();
    let y = x.overflowing_add(105);
    if y.1 & (y.0 < i8::MIN + 3) {
        foo(y.0);
    }
    let y = x.overflowing_add(-105);
    if y.1 & (y.0 > i8::MAX - 3) {
        foo(y.0);
    }
    let y = x.overflowing_sub(-105);
    if y.1 & (y.0 < i8::MIN + 3) {
        foo(y.0);
    }
    let y = x.overflowing_sub(105);
    if y.1 & (y.0 < i8::MAX - 3) {
        foo(y.0);
    }
    let y = x.overflowing_mul(5);
    if y.1 & (y.0 > 100) {
        foo(y.0);
    }
    let y = x.overflowing_mul(-5);
    if y.1 & (y.0 < -100) {
        foo(y.0);
    }
}

#[inline(never)]
fn foo<T>(x: T) {
    core::hint::black_box(x);
}
