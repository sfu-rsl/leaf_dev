use leaf::annotations::Symbolizable;

fn main() {
    let x = 10.mark_symbolic();
    let f = |a: i32, b: i32| -> i32 { a + b };
    let y = f(x, 20);
    if y == 31 {
        foo();
    }
}

fn foo() {}
