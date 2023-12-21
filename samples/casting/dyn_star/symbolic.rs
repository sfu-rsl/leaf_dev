#![feature(dyn_star)]

use runtime::annotations::Symbolizable;

trait Bar {
    fn bar(&self) -> usize;
}

impl Bar for usize {
    fn bar(&self) -> usize {
        *self
    }
}

fn main() {
    let x = 10_usize.mark_symbolic();
    let bar = x as dyn* Bar;
    test(bar);
}

fn test(bar: dyn* Bar) {
    if bar.bar() > 10 {
        foo(20);
    }
}

fn foo<T>(_: T) {}
