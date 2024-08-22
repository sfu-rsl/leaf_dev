use std::cell::Cell;

thread_local! {
    static FOO: Cell<u32> = Cell::new(1);
}

fn main() {
    foo(FOO.get());
}

#[inline(never)]
fn foo<T>(_x: T) {}
