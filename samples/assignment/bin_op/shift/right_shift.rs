#![allow(overflowing_literals)]

fn main() {
    let u1 = 0b1u16 >> 1u16; // == 0
    let u2 = 0b101100u16 >> 3u16; // == 0b101 == 5
    let u3 = 0b1111000011110000u16 >> 15u16; // == 1
    let u4 = 0b111000000000000u16 >> 15u16; // == 0

    // test sign extension
    let us1 = 0b1u16 << 15u16; // == 32768 
    let us2 = us1 >> 15u16; // == 1
    let s1 = 0b1i16 << 15i16; // == -32768
    let s2 = s1 >> 15i16; // == -1

    let x1 = i16::MAX >> 1i16; // == 16383
    let x2 = 0i16 >> 15i16; // == 0

    let a1 = 0b0111010011110010i16 >> 10i16; // == 0b011101 == 29
    let a2 = 0b0111010011110010i16 >> 14i16; // == 1
    let a3 = 0b0111010011110010i16 >> 15i16; // == 0

    let d1 = 0b1i16 >> 15i16; // == 0

    // testing negative numbers
    let d2 = 0b1000000000000001i16 >> 14i16; // == -2
    let d3 = 0b1000000000000001i16 >> 15i16; // == -1
    let d4 = 0b1111011111111111i16 >> 15i16; // == -1

    let e1 = 0b100i128 >> 127i128; // == 0
}
