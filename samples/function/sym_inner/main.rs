use runtime::annotations::Symbolizable;

fn main() {
    let x = (-4).mark_symbolic();
    if bar(x) == 5 {
        foo();
    }
}

fn bar(x: i32) -> i32 {
    1 + calc(x, 3)
}

fn calc(x: i32, y: i32) -> i32 {
    x + y
}

fn foo() {}
