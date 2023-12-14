fn main() {
    let a = foo;
    let b = a as *const ();
}

fn foo() {}
