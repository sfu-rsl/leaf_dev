use std::env;
use tracing_subscriber::{fmt, layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

use compiler::{constants, run_compiler, set_up_compiler};

fn main() {
    init_logging();

    set_up_compiler();

    std::process::exit(run_compiler(
        // The first argument is the executable file name.
        std::env::args().collect::<Vec<_>>().into_iter().skip(1),
        None,
    ));
}

fn init_logging() {
    let env = env::var(constants::LOG_ENV).unwrap_or_default();

    let custom_filter = EnvFilter::builder()
        .with_default_directive(
            format!("{}=off", constants::LOG_PASS_OBJECTS_TAG)
                .parse()
                .unwrap(),
        )
        .with_default_directive(
            format!("{}=off", constants::LOG_PRI_DISCOVERY_TAG)
                .parse()
                .unwrap(),
        )
        .parse_lossy(&env);

    // Create a formatting layer with optional write style based on environment variable
    let fmt_layer = fmt::layer()
        .with_writer(std::io::stderr)
        .with_ansi(env::var(constants::LOG_WRITE_STYLE_ENV).map_or(true, |val| val != "never"));

    // Create a subscriber
    tracing_subscriber::registry()
        .with(custom_filter)
        .with(fmt_layer)
        .init();
}
