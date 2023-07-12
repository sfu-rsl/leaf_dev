use compiler::run_compiler;

fn main() {
    env_logger::init();

    std::process::exit(run_compiler(&mut std::env::args().collect(), None));
}
