// NOTE: The expected behavior is not obvious

fn main() {
    let mut x: u8 = 7;
    loop {
        x *= x;
        if x % 2 == 0 {
            break;
        }
    }

    use_num(x);
}

fn use_num(x: u8) {}
