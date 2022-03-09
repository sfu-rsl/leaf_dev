fn main() {
    let mut x = 1;
    compare(x);
    //println!("x: {x}");
    let mut y = x * 2;
    y += 4;
    dummy0();
    dummy1();
}

fn compare(mut x: i64) {
    x == 1;
    //println!("compare {x}");
}

fn dummy0() {
    let x = 0;
    //println!("dummy0");
}

fn dummy1() {
    let x = 1;
    //println!("dummy1");
}
