fn main() {
    let a: [u8; 3] = [1, 2, 3];
    foo(&a);
}

fn foo(a: &[u8]) -> u8 {
    a[0]
}
