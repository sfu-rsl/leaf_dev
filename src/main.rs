use rc::RunCompiler;

fn main() {
    env_logger::init();

    std::process::exit(RunCompiler::run(&mut std::env::args().collect(), None));
}
