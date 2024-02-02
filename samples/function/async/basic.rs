use std::future::Future;

async fn do_something(x: i32) {}

async fn hello_world() {
    do_something(5).await;
    println!("hello, world!");
}

fn main() {
    let future = hello_world();
    block_on(future);
}

fn block_on(f: impl Future) {
    /* NOTE: Although we are not actually running the future, the above code
     * should be enough for the generation async state machines.
     * To actually run them we can use the `futures` crate. */
}