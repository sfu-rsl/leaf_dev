use leaf::annotations::Symbolizable;

fn main() {
    let z = foo;
    let t = do_clone(&z);

    let mut x = 10.mark_symbolic();
    let c = move |y: i32| test_num(x + y);

    c(0);
    let t = do_clone(&c);
    t(1);
    x += 10;
    // Same behavior as the previous call to `t` is expected as x is captured before.
    let t = do_clone(&c);
    t(1);
}

#[inline(never)]
fn do_clone<T: Clone>(x: &T) -> T {
    <T as Clone>::clone(x)
}

fn test_num(x: i32) {
    if x == 71 {
        foo();
    }
}

fn foo() {}
