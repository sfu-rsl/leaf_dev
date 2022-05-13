fn main() {
    env_logger::init();

    let (exit_code, cb) = rc0::RunCompiler::run(&mut std::env::args().collect(), None);
    log::debug!("{:?}", cb);
    std::process::exit(exit_code)
}
