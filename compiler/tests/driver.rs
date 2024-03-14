use std::fs;
use std::path::Path;
use std::process::Command;

use macros::{gen_tests_rs, gen_tests_toml};

const ENV_LEAFC_LOG: &str = "LEAFC_LOG";
const ENV_RUSTC: &str = "RUSTC";
const ENV_RUST_FLAGS: &str = "RUSTFLAGS";
const ENV_RUST_BACKTRACE: &str = "RUST_BACKTRACE";

const LOG_CONFIG: &str = "off";

// Disable warnings to avoid polluting the output
const RUST_FLAGS: &str = "-Awarnings";

#[gen_tests_rs("samples")]
fn test_compile_rs(fic: &str) {
    let output_dir = create_temp_dir();

    let status = Command::new(env!("CARGO_BIN_EXE_leafc"))
        .arg(get_test_path(fic))
        .arg(RUST_FLAGS)
        .arg("--edition=2021")
        .env(ENV_LEAFC_LOG, LOG_CONFIG)
        .env(ENV_RUST_BACKTRACE, "1")
        .current_dir(&output_dir)
        .status()
        .expect("Failed to spawn and wait for command termination");

    if !status.success() {
        panic!(
            "Failed to compile {} with exit code: {}, output available in {}",
            fic,
            status.code().unwrap_or(-1),
            output_dir.display()
        );
    } else {
        let _ = fs::remove_dir_all(output_dir);
    }
}

#[gen_tests_toml("samples/crates")]
fn test_compile_toml(source_dir: &str) {
    let output_dir = create_temp_dir();

    let status = Command::new("cargo")
        .arg("build")
        .env("CARGO_TARGET_DIR", &output_dir)
        .env(ENV_LEAFC_LOG, LOG_CONFIG)
        .env(ENV_RUSTC, env!("CARGO_BIN_EXE_leafc"))
        .env(ENV_RUST_FLAGS, RUST_FLAGS)
        .env(ENV_RUST_BACKTRACE, "1")
        .current_dir(get_test_path(source_dir))
        .status()
        .expect("Failed to spawn and wait for command termination");

    if !status.success() {
        panic!(
            "Failed to compile {} with exit code: {}, output available in {}",
            source_dir,
            status.code().unwrap_or(-1),
            output_dir.display()
        );
    } else {
        let _ = fs::remove_dir_all(output_dir);
    }
}

fn create_temp_dir() -> std::path::PathBuf {
    use std::time::SystemTime;
    let mut path = std::env::temp_dir();
    path.push(
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
            .to_string(),
    );
    fs::create_dir_all(&path).unwrap();
    path
}

fn get_test_path(name: &str) -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join(name)
}
