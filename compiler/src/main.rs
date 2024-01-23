use compiler::run_compiler;

fn main() {
    env_logger::Builder::new()
        .filter_module(
            compiler::constants::LOG_PASS_OBJECTS_TAG,
            log::LevelFilter::Off,
        )
        .filter_module(
            compiler::constants::LOG_PRI_DISCOVERY_TAG,
            log::LevelFilter::Off,
        )
        .filter_module(
            &stringify!(compiler::mir_transform::jump).replace(" ", ""),
            log::LevelFilter::Off,
        )
        .parse_default_env()
        .init();

    std::process::exit(run_compiler(
        // The first argument is the executable file name.
        std::env::args().collect::<Vec<_>>().into_iter().skip(1),
        None,
    ));
}
