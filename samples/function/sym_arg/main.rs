use runtime::annotations::Symbolizable;

fn main() {
    let x = 10.mark_symbolic();
    check(x, 5);
}

fn calc(x: i32, y: i32) {
    if x < y {
        foo();
    } else {
        bar();
    }
}

fn foo() {}

fn bar() {}
