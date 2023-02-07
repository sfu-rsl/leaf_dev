fn main() {
    let mut a = get_num();
    check_num(&a);
    increase_num(&mut a);
}

fn get_num() -> i32 {
    10
}

fn check_num(x: &i32) -> bool {
    x % 2 == 0
}

fn increase_num(x: &mut i32) {
    *x += 5;
}