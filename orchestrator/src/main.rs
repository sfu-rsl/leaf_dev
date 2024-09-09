use std::path::PathBuf;

use common::log_info;

mod modes;

const LOG_ENV: &str = "LEAFO_LOG";

fn main() {
    init_logging();

    log_info!("Starting the orchestrator");
    test_in_pure_concolic_mode("program", "Welcome!");
}

fn init_logging() {
    use tracing_subscriber::{fmt, layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};
    let env_filter = EnvFilter::builder().parse_lossy(&std::env::var(LOG_ENV).unwrap_or_default());

    let fmt_layer = fmt::layer().with_writer(std::io::stderr);

    tracing_subscriber::registry()
        .with(env_filter)
        .with(fmt_layer)
        .init();
}

fn test_in_pure_concolic_mode(program: &str, search_keyword: &str) {
    let options = modes::div_path::Options {
        program: PathBuf::from(program),
        child_env: {
            let mut env = std::collections::HashMap::new();
            env.insert("LEAF_LOG".to_string(), "warn".to_string());
            env
        },
        current_inputs_dir: PathBuf::from("loop/current"),
        past_inputs_dir: PathBuf::from("loop/past"),
        next_inputs_dir: PathBuf::from("loop/next"),
    };
    modes::div_path::run_loop(options, |output| {
        String::from_utf8_lossy(&output.stdout).contains(search_keyword)
    });
}
