// Currently, we haven't added the support for transitive dependencies. Thus,
// the logger library doesn't work.

macro_rules! log_info {
    // ($($arg:tt)+) => (log::info!($($arg)+))
    ($($arg:tt)+) => (println!($($arg)+))
}

macro_rules! log_debug {
    // ($($arg:tt)+) => (log::debug!($($arg)+))
    ($($arg:tt)+) => (println!($($arg)+))
}

pub(crate) use {log_debug, log_info};
