fn main() {
    env_logger::init();

    std::process::exit(leafc::RunCompiler::run(&mut std::env::args().collect(), None, false).0);
}
