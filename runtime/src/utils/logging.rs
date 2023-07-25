#[deprecated(note = "Use `log::info!` instead")]
macro_rules! log_info {
    ($($arg:tt)+) => (log::info!($($arg)+))
}

#[deprecated(note = "Use `log::debug!` instead")]
macro_rules! log_debug {
    ($($arg:tt)+) => (log::debug!($($arg)+))
}

pub(crate) use {log_debug, log_info};

pub(crate) fn init_logging() {
    use env_logger::Env;
    const ENV_LOG: &str = "LEAF_LOG";
    const ENV_WRITE_STYLE: &str = "LEAF_LOG_STYLE";
    env_logger::init_from_env(Env::new().filter(ENV_LOG).write_style(ENV_WRITE_STYLE));
    log::debug!("Logging initialized");
}
