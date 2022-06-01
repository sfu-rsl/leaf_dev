#![allow(unused)]
fn main() {
    struct RectangularArray<T, const WIDTH: usize, const HEIGHT: usize> {
        array: [[T; WIDTH]; HEIGHT],
    }

    let a = RectangularArray::<isize, 2, 3> {
        array: [[isize::default(), 2]; 3],
    };
}
