pub(crate) fn init_logging() {
    use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};
    use tracing_tree::{time::FormatTime, HierarchicalLayer};

    const ENV_LOG: &str = "LEAF_LOG";
    const ENV_WRITE_STYLE: &str = "LEAF_LOG_STYLE";

    let env = std::env::var(ENV_LOG).unwrap_or_default();
    #[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
    struct ChronoLocalDateTime;

    impl FormatTime for ChronoLocalDateTime {
        fn format_time(&self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
            let time = chrono::Local::now();
            write!(w, "{}", time.format("%Y-%m-%d %H:%M:%S"))
        }

        fn style_timestamp(
            &self,
            _ansi: bool,
            _elapsed: std::time::Duration,
            _w: &mut impl std::fmt::Write,
        ) -> std::fmt::Result {
            Ok(())
        }
    }

    let fmt_layer = HierarchicalLayer::default()
        .with_ansi(std::env::var(ENV_WRITE_STYLE).map_or(true, |val| val != "false"))
        .with_writer(std::io::stdout)
        .with_thread_names(false)
        .with_bracketed_fields(false)
        .with_verbose_exit(false)
        .with_verbose_entry(false)
        .with_timer(ChronoLocalDateTime)
        .with_deferred_spans(false)
        .with_targets(true);

    let default_directive = format!("pri=off,leafrt::backends=off,z3=off,{}", env);
    let env_filter = EnvFilter::builder().parse_lossy(default_directive);

    tracing_subscriber::registry()
        .with(env_filter)
        .with(fmt_layer)
        .try_init()
        .unwrap();
}
