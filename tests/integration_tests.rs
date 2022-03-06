use rc::RunCompiler;
use std::path::PathBuf;
use test_log::test;

macro_rules! gen_test {
    ( $x:ident ) => {
        #[test]
        fn $x() {
            call_compiler(&format!("{}.rs", stringify!($x)));
        }
    };
}

gen_test!(test0);
gen_test!(test1);

fn call_compiler(test_file: &str) {
    let args: Vec<_> = std::env::args().collect();
    let mut args: Vec<String> = args[..1].into();
    args.push(String::from("--out-dir"));
    args.push(String::from(env!("CARGO_TARGET_TMPDIR")));
    RunCompiler::run(
        &mut args,
        Some(PathBuf::from(format!(
            "{}{}{}",
            env!("CARGO_MANIFEST_DIR"),
            "/tests/test_cases/",
            test_file
        ))),
    );
}
