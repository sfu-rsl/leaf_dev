fn main() {
    tupler(1, 2);
}

fn tupler<T1, T2>(a: T1, b: T2) {
    foo((a, b));
}

fn foo<T>(x: T) {}
