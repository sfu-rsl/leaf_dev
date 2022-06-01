fn main() {
    env_logger::init();

    std::process::exit(rc0::RunCompiler::run(&mut std::env::args().collect(), None));
}
