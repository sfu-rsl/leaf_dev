#![allow(unused_variables)]

use std::hint::black_box;

const A: i32 = 20;

fn main() {
    let b = black_box(true);

    let si = black_box(A);

    let si = black_box(-10_i8);
    let si = black_box(-10_i16);
    let si = black_box(-10_i32);
    let si = black_box(-10_i64);
    let si = black_box(-10_i128);

    let ui = black_box(10_u8);
    let ui = black_box(10_u16);
    let ui = black_box(10_u32);
    let ui = black_box(10_u64);
    let ui = black_box(10_u128);

    let f = black_box(10.33_f32);
    let f = black_box(10.33_f64);

    let c = black_box('a');

    let s = black_box("Hello, world!");

    let b = black_box(b"Hello, world!");
}
