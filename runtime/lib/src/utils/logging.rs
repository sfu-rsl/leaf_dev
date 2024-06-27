use common::log_debug;

pub(crate) fn init_logging() {
    use tracing_subscriber::{fmt, layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

    const ENV_LOG: &str = "LEAF_LOG";
    const ENV_WRITE_STYLE: &str = "LEAF_LOG_STYLE";

    let env = std::env::var(ENV_LOG).unwrap_or_default();

    let env_filter = EnvFilter::builder()
        .with_default_directive("z3=off".parse().unwrap())
        .parse_lossy(&env);

    let fmt_layer = fmt::layer()
        .with_writer(std::io::stdout)
        .with_ansi(std::env::var(ENV_WRITE_STYLE).map_or(true, |val| val != "never"));

    let _ = tracing_subscriber::registry()
        .with(env_filter)
        .with(fmt_layer)
        .try_init();

    log_debug!("Logging initialized");
}
