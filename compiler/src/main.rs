use compiler::run_compiler;

fn main() {
    env_logger::init();

    std::process::exit(run_compiler(&std::env::args().collect::<Vec<_>>(), None));
}
