//! Benchmarks the performance of the `std::fmt` module.
//! The benchmarks here are not very comprehensive and should be eventually
//! replaced with more "real-world" code.

use benchlib::benchmark::run_benchmark_group;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
struct Person {
    first_name: &'static str,
    last_name: &'static str,
    age: u8,
}

fn main() {
    let persons: Vec<_> = make_persons();

    run_benchmark_group(|group| {
        group.register_benchmark("fmt-write-str", || {
            const CONST_STRING: &str = "foobar";
            let mut buffer = String::with_capacity(256 * 1024 * 1024);
            || {
                let iterations = 500000;
                for i in 0..iterations {
                    write!(buffer, "Iteration {i} out of {iterations}: {CONST_STRING}").unwrap();
                }
                buffer
            }
        });
        group.register_benchmark("fmt-debug-derive", || {
            let mut buffer = String::with_capacity(256 * 1024 * 1024);
            || {
                write!(buffer, "{persons:?}").unwrap();
                buffer
            }
        });
    });
}

include!("persons.rs");
fn make_persons() -> Vec<Person> {
    PERSONS.to_vec()
}

