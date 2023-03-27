#![allow(unused_variables)]

fn main() {
    let a = get_int();
    let c = a as u32; // IntToInt

    let g = get_foo();
    let h = g as u32; // IntToInt

    let i = get_bool();
    let j = i as u32; // IntToInt

    let k = get_char();
    let l = k as u32; // IntToInt

    let m = get_u8();
    let n = m as char; // IntToInt
}

fn get_int() -> isize {
    10
}

#[allow(dead_code)]
enum Foo {
    Bar,
    Baz,
}

fn get_foo() -> Foo {
    Foo::Bar
}

fn get_bool() -> bool {
    true
}

fn get_char() -> char {
    'a'
}

fn get_u8() -> u8 {
    10
}
