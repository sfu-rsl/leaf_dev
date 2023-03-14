// NOTE: this doesn't count as array repeat syntax
struct A<const N: usize> {
    a: [u32; N],
}

fn main() {
    let x: A::<43>;
}
