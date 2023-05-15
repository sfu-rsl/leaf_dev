use runtime::annotations::Symbolizable;

fn main() {
    let x = 10.mark_symbolic();
    let a = calc(x, 5);
    if a == 15 {
        foo();
    }
}

fn calc(x: i32, y: i32) -> i32 {
    x + y
}

fn foo() {}
