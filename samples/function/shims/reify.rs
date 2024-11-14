use leaf::annotations::Symbolizable;

fn main() {
    let x = 10.mark_symbolic();
    let foo = Foo { y: 20 };
    let t = &foo as &dyn TestTrait;
    let f = <dyn TestTrait as TestTrait>::test as *const ();
    let f: fn(&dyn TestTrait, i32) = unsafe { core::mem::transmute(f) };
    f(t, x);
}

trait TestTrait {
    fn test(&self, x: i32);
}

struct Foo {
    y: i32,
}

impl TestTrait for Foo {
    fn test(&self, x: i32) {
        if x == self.y {
            core::hint::black_box(());
        }
    }
}
