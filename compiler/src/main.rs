use compiler::run_compiler;

fn main() {
    env_logger::Builder::new()
        .filter_module(
            compiler::constants::LOG_PASS_OBJECTS_TAG,
            log::LevelFilter::Off,
        )
        .parse_default_env()
        .init();

    std::process::exit(run_compiler(&std::env::args().collect::<Vec<_>>(), None));
}
