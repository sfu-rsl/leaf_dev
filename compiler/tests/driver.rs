use std::fs;
use std::path::Path;
use std::process::Command;

use macros::gen_tests;

#[gen_tests("samples")]
fn test_compile(fic: &str) {
    let output_dir = {
        use std::time::SystemTime;
        let mut path = std::env::temp_dir();
        path.push(
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
                .to_string(),
        );
        path
    };
    fs::create_dir_all(&output_dir).unwrap();

    let status = Command::new(env!("CARGO_BIN_EXE_leafc"))
        .arg(
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .join(fic),
        )
        // Disable warnings to avoid polluting the output
        .arg("-Awarnings")
        .env("RUST_LOG", "off")
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
