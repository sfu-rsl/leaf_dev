#![feature(ptr_metadata)]

use std::{pin::Pin, rc::Rc, sync::Arc};

use leaf::annotations::Symbolizable;

fn main() {
    let mut foo = Foo { y: 20 };

    macro_rules! test {
        ($($method:ident -> $receiver:expr),+$(,)?) => {
            $(
                let x = 10.mark_symbolic();
                let r = $receiver;
                <dyn TraitMethods as TraitMethods>::$method(r, x);
            )+
        };
    }

    test!(
        by_ref -> &foo,
        by_ref_mut -> &mut foo,
        by_box -> Box::new(foo),
        by_rc -> Rc::new(foo),
        by_arc -> Arc::new(foo),
        by_pin -> Pin::new(&foo),
        with_lifetime -> &foo,
        nested_pin -> Pin::new(Arc::new(foo)),
    );
}

#[derive(Clone, Copy)]
struct Foo {
    y: i32,
}

impl Foo {
    fn test(&self, x: i32) {
        if x == self.y {
            core::hint::black_box(());
        }
    }
}

// Examples of object safe methods.
// Ref: https://doc.rust-lang.org/reference/items/traits.html
trait TraitMethods {
    fn by_ref(self: &Self, x: i32);
    fn by_ref_mut(self: &mut Self, x: i32);
    fn by_box(self: Box<Self>, x: i32);
    fn by_rc(self: Rc<Self>, x: i32);
    fn by_arc(self: Arc<Self>, x: i32);
    fn by_pin(self: Pin<&Self>, x: i32);
    fn with_lifetime<'a>(self: &'a Self, x: i32);
    fn nested_pin(self: Pin<Arc<Self>>, x: i32);
}

impl TraitMethods for Foo {
    fn by_ref(self: &Self, x: i32) {
        self.test(x)
    }
    fn by_ref_mut(self: &mut Self, x: i32) {
        self.test(x)
    }
    fn by_box(self: Box<Self>, x: i32) {
        self.test(x)
    }
    fn by_rc(self: Rc<Self>, x: i32) {
        self.test(x)
    }
    fn by_arc(self: Arc<Self>, x: i32) {
        self.test(x)
    }
    fn by_pin(self: Pin<&Self>, x: i32) {
        self.test(x)
    }
    fn with_lifetime<'a>(self: &'a Self, x: i32) {
        self.test(x)
    }
    fn nested_pin(self: Pin<Arc<Self>>, x: i32) {
        self.test(x)
    }
}
