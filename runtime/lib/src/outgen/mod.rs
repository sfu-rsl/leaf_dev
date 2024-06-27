use std::{
    collections::HashMap,
    fmt::{Display, Write},
    hash::Hash,
};

use crate::abs::backend::OutputGenerator;
use common::{log_debug, log_info, log_warn};

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
        log_info!("Solver returned a solution:\n{answers_str}");
    }
}
