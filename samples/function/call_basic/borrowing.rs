fn main() {
    let a = Bar { x: 20 };
    check(&a);
}

fn check(b: &Bar) {
    if b.x > 10 {
        foo(b);
    }
}

fn foo<T>(x: T) {}

struct Bar {
    x: i32,
}
