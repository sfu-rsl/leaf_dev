use std::fmt::Debug;

use crate::{abs::backend::OutputGenerator, utils::logging::log_info};

pub(crate) struct LoggerOutputGenerator;

impl<I: Debug, V: Debug> OutputGenerator<I, V> for LoggerOutputGenerator {
    fn generate(&mut self, values: Vec<(&I, &V)>) {
        log_info!("Found a solution:");

        for (key, value) in values {
            log_info!("{:#?} = {:#?}", key, value);
        }
    }
}
