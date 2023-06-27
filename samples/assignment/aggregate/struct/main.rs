fn main() {
    let a = get_foo();
    let b = get_bar();
    let c = Baz { a, b };
}

fn get_num() -> i32 {
    10
}

fn get_foo() -> Foo {
    Foo { x: 5, y: get_num() }
}

fn get_bar() -> Bar {
    Bar(5, get_num())
}

struct Foo {
    x: i32,
    y: i32,
}

struct Bar(i32, i32);

struct Baz {
    a: Foo,
    b: Bar,
}
