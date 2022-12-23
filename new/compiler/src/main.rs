
fn main() {
    env_logger::init();

    std::process::exit(compiler::RunCompiler::run(&mut std::env::args().collect(), None, false));
}
