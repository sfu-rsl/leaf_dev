#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

use std::ops::Coroutine;
use std::pin::Pin;

fn main() {
    let mut coroutine = #[coroutine]
    |x: u32| {
        yield x;
        return x + 10;
    };

    Pin::new(&mut coroutine).resume(1);
    Pin::new(&mut coroutine).resume(2);
}
