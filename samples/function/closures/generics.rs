/* Closures inherit their parent fn generics, even if they don't use it.
 * In this case, the closures in `consume_bar` are generic over `T`, and `f` is
 * truly generic over `T`.
 */

fn main() {
    let b = FooBar;
    consume_bar(b);
}

fn consume_bar<T: Bar>(b: T) {
    let f = |x: T| x.bar();
    f(b);
    let g = || foo();
    g();
}

trait Bar {
    fn bar(&self);
}

struct FooBar;

impl Bar for FooBar {
    fn bar(&self) {
        foo();
    }
}

fn foo() {}
