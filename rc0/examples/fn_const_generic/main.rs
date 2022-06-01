#![allow(unused)]
fn main() {
    const_generic::<2>();
}

fn const_generic<const T: usize>() {
    let x = T * 2;
    println!("T: {}", T);
}
