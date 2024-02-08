use std::env;
use std::path::Path;

// This file's current purpose is to streamline z3 installation on windows

fn main() -> std::io::Result<()> {
    if cfg!(target_os = "windows") {
        let path = std::env::var("Z3_SYS_Z3_HEADER")
            .expect("Z3_SYS_Z3_HEADER environment variable not set!");
        let path = Path::new(&path)
            .parent()
            .expect("Z3_SYS_Z3_HEADER environment variable not well formed")
            .join("..")
            .join("bin");

        let out_dir = env::var("OUT_DIR").unwrap();
        let out_dir = Path::new(&out_dir)
            .join("..")
            .join("..")
            .join("..")
            .join("deps");

        std::fs::copy(path.join("libz3.dll"), out_dir.join("libz3.dll"))?;
        std::fs::copy(path.join("libz3.lib"), out_dir.join("libz3.lib"))?;
        std::fs::copy(path.join("libz3.pdb"), out_dir.join("libz3.pdb"))?;
    }

    Ok(())
}
