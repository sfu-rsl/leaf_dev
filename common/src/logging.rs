pub use tracing;

#[macro_export]
macro_rules! log_info {
    ($($arg:tt)+) => ($crate::logging::tracing::info!($($arg)+))
}

#[macro_export]
macro_rules! log_debug {
    ($($arg:tt)+) => ($crate::logging::tracing::debug!($($arg)+))
}

#[macro_export]
macro_rules! log_warn {
    ($($arg:tt)+) => ($crate::logging::tracing::warn!($($arg)+))
}
