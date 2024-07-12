fn main() {
    let a = get_foo_a();
    let b = get_foo_b();
}

fn get_foo_a() -> Foo {
    Foo { a: 5 }
}

fn get_foo_b() -> Foo {
    Foo { b: 10 }
}

union Foo {
    a: i32,
    b: i64,
}
