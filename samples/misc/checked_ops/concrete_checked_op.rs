fn main() {
    // testing i16 with all the operations
    let mut x: i16 = num() as i16;
    let y = x + (-1i16);
    let z = y * 20;
    let w = z - 1000;

    // testing i64 close to overflowing
    let x2: i64 = num() as i64;
    let y2 = x2 + 1000;
    let z2 = y2 * 20000000;
    let w2 = z2 + (1i64 << 63i64) + (1i64 << 62);

    // testing the edge of i64 & left shifting to negatives & positives
    let t1 = -10 + (1i64 << 62i64);
    let t2 = 10 + (1i64 << 63i64);
    let t3 = -10 + (1i128 << 64i128);
    let t4 = -10 + (1i128 << 65i128);

    // testing i128 close to overflowing
    let x3: i128 = num() as i128;
    let w3 = x3 + (1i128 << 125);
    let a3 = w3 + (1i128 << 127);
    let b3 = a3 - (1i128 << 127);
    let c3 = b3 - (1i128 << 126);
}

fn num() -> i16 {
    20
}
