#![allow(unused)]
fn main() {
    let mut test0 = Test::new();
    test0.test0();

    let mut test1 = test0;
    test1.test0();
}

struct Test {
    f0: u32,
    f1: char,
    f2: &'static str,
}

impl Test {
    fn new() -> Test {
        Test {
            f0: 1,
            f1: 'c',
            f2: "abcd",
        }
    }

    fn test0(&mut self) {
        self.f0 = 2;
        self.f1 = 'b';
        self.f2 = "efgh";
    }
}
