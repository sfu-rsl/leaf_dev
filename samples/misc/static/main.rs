static PRIMITIVE: i32 = 10;

static PRIMITIVE_REF: &i32 = &PRIMITIVE;

static ADT: TestStruct = TestStruct { a: 7, b: 11 };

static ADT_REF: &TestStruct = &ADT;

// Not supported yet
// static FN_PTR: fn() -> () = bar;

fn main() {
    foo(PRIMITIVE);
    foo(PRIMITIVE_REF);
    foo(ADT);
    foo(&ADT);
    foo(ADT_REF);

    // Not supported yet
    // foo(FN_PTR);
}

#[derive(Clone, Copy)]
struct TestStruct {
    a: i32,
    b: i32,
}

fn foo<T>(x: T) {
    bar();
}

fn bar() {}
