use runtime::annotations::Symbolizable;

fn main() {
    let x = 10.mark_symbolic();
    let a = calc(x, 5);
    let y = 20.mark_symbolic();
    let b = calc(y, 3);

    // Using bitwise AND instead of logical AND to make them appear in one branch.
    if (a == 15) & (b == 25) {
        foo();
    }
}

fn calc(x: u32, y: u32) -> u32 {
    x + y
}

fn foo() {}
