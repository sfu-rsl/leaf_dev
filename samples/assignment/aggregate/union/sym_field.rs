use leaf::annotations::Symbolizable;

fn main() {
    let a = get_foo_a();
    let b = get_foo_b();

    if unsafe { a.a } == 5 && unsafe { b.b } == 11 {
        foo();
    }
}

fn get_foo_a() -> Foo {
    Foo {
        a: 5.mark_symbolic(),
    }
}

fn get_foo_b() -> Foo {
    Foo {
        b: 10.mark_symbolic(),
    }
}

union Foo {
    a: i32,
    b: i64,
}

fn foo() {}
