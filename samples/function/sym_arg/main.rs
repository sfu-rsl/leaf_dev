use leaf::annotations::Symbolizable;

fn main() {
    let x = 10.mark_symbolic();
    calc(x, 5);
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
