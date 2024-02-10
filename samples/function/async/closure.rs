#![feature(async_fn_traits)]
#![feature(async_closure)]

use std::ops::AsyncFn;

async fn do_something(x: i32) {}

fn main() {
    let future = async || { 
        do_something(5).await;
        println!("hello, world!");
    };
    block_on(future);
}

fn block_on(f: impl AsyncFn() -> ()) {
    /* NOTE: Although we are not actually running the future, the above code
     * should be enough for the generation async state machines.
     * To actually run them we can use the `futures` crate. */
}
