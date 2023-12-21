#![feature(dyn_star)]

use runtime::annotations::Symbolizable;

trait Bar {
    fn bar(&self) -> usize;
}

struct Foo(usize);

impl Bar for Foo {
    fn bar(&self) -> usize {
        self.0
    }
}

fn main() {
    let x = Foo(10.mark_symbolic());
    let bar = x as dyn* Bar;
    test(bar);
}

fn test(bar: dyn* Bar) {
    if bar.bar() > 10 {
        foo(20);
    }
}

fn foo<T>(_: T) {}
