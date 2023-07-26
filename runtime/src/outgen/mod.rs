use std::fmt::Display;

use crate::{abs::backend::OutputGenerator, utils::logging::log_info};

pub(crate) struct LoggerOutputGenerator;

impl<I: Display, V: Display> OutputGenerator<I, V> for LoggerOutputGenerator {
    fn generate(&mut self, values: Vec<(&I, &V)>) {
        log::info!(
            "Found a solution:\n{:#?}",
            values
                .iter()
                .map(|(key, value)| format!("{} = {}", key, value))
                .collect::<Vec<_>>()
        );
    }
}
