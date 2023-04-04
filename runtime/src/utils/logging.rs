macro_rules! log_info {
    // Currently, we haven't added the support for transitive dependencies. Thus,
    // the logger library doesn't work.
    // ($($arg:tt)+) => (log::info!($($arg)+))
    ($($arg:tt)+) => (println!($($arg)+))
}

pub(crate) use log_info;
