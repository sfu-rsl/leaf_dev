use std::{
    collections::HashMap,
    fmt::{Display, Write},
    hash::Hash,
};

use crate::abs::backend::OutputGenerator;

pub(crate) struct LoggerOutputGenerator;

impl<I: Display + Ord + Hash, V: Display> OutputGenerator<I, V> for LoggerOutputGenerator {
    fn generate(&mut self, answers: HashMap<I, V>) {
        let mut answers_str = String::new();
        writeln!(answers_str, "{{").unwrap();
        let mut ids = answers.keys().collect::<Vec<_>>();
        ids.sort();
        for i in ids {
            writeln!(answers_str, "    \"{}\": {},", i, answers[i]).unwrap();
        }
        writeln!(answers_str, "}}").unwrap();
        log::info!("Found a solution:\n{answers_str}");
    }
}
