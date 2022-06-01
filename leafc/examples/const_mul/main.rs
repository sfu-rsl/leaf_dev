#![allow(unused)]

fn main() {
    let mut x = 1;
    noop(x);
    let a = 2;
    let mut y = x * a;
    //let mut y = x * 2;
    noop(y);
    let z = y * 3 * (x * 4);
    let z = 2 * 2;
}

fn noop(mut i: i32) {}
