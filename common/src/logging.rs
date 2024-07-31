pub use tracing;

#[macro_export]
macro_rules! log_error {
    ($($arg:tt)+) => ($crate::logging::tracing::error!($($arg)+))
}

#[macro_export]
macro_rules! log_trace {
    ($($arg:tt)+) => ($crate::logging::tracing::trace!($($arg)+))
}

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
