// NOTE: this will fail until generic functions are implemented

fn main() {
    bar::<43>();
}

fn bar<const M: usize>() {
    let x = 42_u32;
    let a = [x; M];
}