use runtime::annotations::Symbolizable;

#[allow(unused_variables)]

fn main() {
    let a = get_int().mark_symbolic();
    let b = a as u32;

    let c = get_uint().mark_symbolic();
    let d = c as i64;

    let e = get_uint().mark_symbolic();
    let f = e as u8;

    let i = get_bool().mark_symbolic();
    let j = i as u8;

    let k = get_char().mark_symbolic();
    let l = k as u64;

    // this is considered a char cast in the runtime (non-integer)
    let m = get_u8().mark_symbolic();
    let n = m as char;
}

fn get_int() -> isize {
    -10
}

fn get_uint() -> usize {
    1000
}

fn get_bool() -> bool {
    true
}

fn get_char() -> char {
    'a'
}

fn get_u8() -> u8 {
    65 // ASCII 'A'
}
