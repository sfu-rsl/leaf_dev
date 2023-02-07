fn main() {
    let mut a = get_num();
    let x: *const i32 = &a;
    let x: *mut i32 = &mut a;
}

fn get_num() -> i32 {
    10
}