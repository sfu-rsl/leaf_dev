fn main() {
    let a: [u8; 3] = [1, 2, 3];
    foo(&a);
}

fn foo(_a: &[u8]) { // Pointer(Unsize) cast happens here to turn the array into a slice
}
