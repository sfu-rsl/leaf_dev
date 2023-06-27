fn main() {
    // testing i16 with all the operations
    let mut x: i16 = num() as i16;
    let y = x + 10;
    let z = y * 20;
    let w = z - 1000;

    // testing i64 close to overflowing
    let x2: i64 = num2() as i64;
    let y2 = x2 + 1000;
    let z2 = y2 * 20000000;
    let w2 = z2 + (1i64 << 63) + (1i64 << 62);
    //let a2 = w2 + (1i64 << 62); // this operation should overflow

    // testing i128 close to overflowing, and actually underflowing
    let x3: i128 = num3() as i128;
    let w3 = z3 + (1i128 << 125);
    let a3 = w3 + (1i128 << 127);
    let b3 = a3 - (1i128 << 127);
    let c3 = b3 - (1i128 << 127);
    let d3 = c3 - (1i128 << 127); // this operation should overflow
}

fn num() -> i16 {
    20
}
