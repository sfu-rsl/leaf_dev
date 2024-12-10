use leaf::annotations::*;

fn main() {
    let x: u8 = 10.mark_symbolic();

    if x < 5 {
        println!("Hello, world!");
    }
}
