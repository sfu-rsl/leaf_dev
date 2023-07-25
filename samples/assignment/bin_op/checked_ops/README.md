# unsigned_test.rs
 in order to build with overflow checks off (for testing purposes), use the following command:
 - `cargo run -- --emit=mir .\unsigned_test.rs -C overflow-checks=off` to generate mir
 - or `cargo run -- .\unsigned_test.rs -C overflow-checks=off` to generate a binary