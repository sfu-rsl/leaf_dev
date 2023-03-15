fn main() {
    let mut x: usize = 10;
    while x < 50 {
        x += x-9 / 3;
        x += 2;
    }
}