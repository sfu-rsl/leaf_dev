
fn main() {
    env_logger::init();

    std::process::exit(leafc_new::RunCompiler::run(&mut std::env::args().collect(), None, false));
}
