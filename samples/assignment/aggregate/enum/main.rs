fn main() {
    let a = get_foo();
    let b = get_bar();
    let c = get_baz();
    let d = get_niche();
}

fn get_foo() -> Test {
    Test::Foo { x: 5, y: 10 }
}

fn get_bar() -> Test {
    Test::Bar(5, 10)
}

fn get_baz() -> Test {
    Test::Baz { a: 5 }
}

fn get_niche() -> TestNiche {
    TestNiche::Third
}

enum Test {
    Foo { x: i32, y: i32 },
    Bar(i32, i32),
    Baz { a: i32 },
}

enum TestNiche {
    First,
    Second(Test),
    Third,
}
