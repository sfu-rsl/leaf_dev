fn main() {
    tupler(1_u16, 2_i16); // expect "(u16, i16)" in type export
}

fn tupler<T1, T2>(a: T1, b: T2) {
    foo((a, b));
}

fn foo<T>(x: T) {}
