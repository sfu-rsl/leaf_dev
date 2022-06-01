#![allow(unused)]

const T: i32 = 1234;
const T1: u64 = 1234;
const T2: bool = true;
const T3: &str = "T3";

fn main() {
    let x = &&1;
    let x = &T;
    let x = &T1;
    let x = &T2;
    let x = &T3;
    let x = 1_usize;
    let x = "x";
    //noop(x);
    //let mut y = [&&&&&&&&1; 10];
    //let (x, y) = (&1, [&&&&&&&&1; 10]);
    //noop(x);
    //let mut x = 1;
    //noop(x);
    //let mut y = x * 2;
    //noop(y);
    //let z = y * 3 * (x * 4);
    //let z = 2 * 2;
}

//fn noop<T>(mut i: T) {}
