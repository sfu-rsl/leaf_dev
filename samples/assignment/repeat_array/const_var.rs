fn main() {
    let x = 42_u32;
    const A: usize = 43;
    const B: usize = A;
    const C: usize = B;
    let a = [x; C];
}