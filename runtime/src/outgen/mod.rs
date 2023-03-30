use std::fmt::Debug;

use crate::abs::backend::OutputGenerator;

struct LoggerOutputGenerator;

impl<I: Debug, V: Debug> OutputGenerator<I, V> for LoggerOutputGenerator {
    fn generate(&mut self, values: Vec<(I, V)>) {
        println!("Found a solution:");
        
        for (key, value) in values {
            println!("{:?} = {:?}", key, value);
        }
    }
}
