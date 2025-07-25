const LOG_ENV: &str = "LEAFO_LOG";

pub fn init_logging() {
    use tracing_subscriber::{EnvFilter, fmt, layer::SubscriberExt, util::SubscriberInitExt};
    let env_filter = EnvFilter::builder()
        .with_env_var(LOG_ENV.to_string())
        .from_env_lossy();

    #[allow(unused)]
    let fmt_layer = tracing_tree::HierarchicalLayer::default()
        .with_timer(tracing_tree::time::LocalDateTime::default())
        .with_targets(true);
    let fmt_layer = fmt::layer().with_writer(std::io::stderr);

    let indicatif_layer = tracing_indicatif::IndicatifLayer::new();

    tracing_subscriber::registry()
        .with(env_filter)
        .with(fmt_layer)
        .with(indicatif_layer)
        .init();
}
