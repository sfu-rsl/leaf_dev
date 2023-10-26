use runtime::annotations::Symbolizable;

fn main() {
    let foo = Foo {
        x: 10.mark_symbolic(),
        y: 2.mark_symbolic(),
    };
    if (foo.x < 10) & (foo.x > 5) {
        error();
    }
    let bar = Bar { foo, z: 3 };
    check_bar(bar);
}

fn check_bar(bar: Bar) {
    if bar.foo.x < bar.z {
        error();
    }
}

fn error() {}

struct Foo {
    x: u32,
    y: u32,
}

struct Bar {
    foo: Foo,
    z: u32,
}
