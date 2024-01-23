use leaf::annotations::Symbolizable;

mod child;

use child::calc;

fn main() {
    let x = 0.mark_symbolic();
    calc(x, 5);
}

fn foo() {}

fn bar() {}
