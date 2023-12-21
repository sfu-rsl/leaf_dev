#![feature(dyn_star)]

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
    let x = Foo(10);
    let bar = x as dyn* Bar;
    foo(bar);
}

fn foo<T>(_: T) {}
