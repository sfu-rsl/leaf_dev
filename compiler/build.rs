#![feature(exit_status_error)]

use std::{
    env, fs,
    path::{Path, PathBuf},
};

const SHIM_LIB_PROJECT_DIR: [&str; 2] = ["runtime", "shim"];
const SHIM_LIB_FILE_NAME: &str = "libleafrtsh.rlib";

const PATH_TOOLCHAIN_BUILDER: [&str; 2] = ["scripts", "build_core"];
const FILE_TOOLCHAIN_BUILDER: &str = "toolchain_builder";

const ENV_SHIM_LIB_LOCATION: &str = "SHIM_LIB_LOCATION";
const ENV_WORKSPACE_DIR: &str = "WORKSPACE_DIR";

fn main() {
    let workspace_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .parent()
        .unwrap()
        .to_path_buf();

    println!(
        "cargo:rustc-env={ENV_WORKSPACE_DIR}={}",
        workspace_dir.display(),
    );

    let shim_lib_location = provide_runtime_shim_lib(&workspace_dir);
    println!(
        "cargo:rustc-env={ENV_SHIM_LIB_LOCATION}={}",
        shim_lib_location.display(),
    );

    provide_toolchain_builder(&workspace_dir);
}

fn provide_runtime_shim_lib(workspace_dir: &Path) -> PathBuf {
    /* NOTE: Why are we building it separately and not using the workspace's target output.
     * When building through workspace, if the shim library shares a dependency
     * with other projects, cargo builds a single version of that dependency,
     * which works for all. In our case, common library is built with additional
     * features not required for the runtime shim library.
     * Furthermore, this guarantees a more controlled and isolated build with
     * the necessary dependencies being added to the target programs, otherwise
     * it will be hard to separate those.
     * In short, an isolated build is achieved in this way.
     */
    use runtime_shim::*;

    let shim_lib_proj_path = workspace_dir.join(PathBuf::from_iter(SHIM_LIB_PROJECT_DIR));
    println!("cargo:rerun-if-changed={}", shim_lib_proj_path.display());

    let target_output_dir = build_lib(&shim_lib_proj_path);

    let shim_as_dep_path = copy_lib_to_deps(&target_output_dir);
    println!("cargo:rerun-if-changed={}", shim_as_dep_path.display());

    shim_as_dep_path
}

mod runtime_shim {
    use super::*;

    use std::process::Command;

    const DIR_DEPS: &str = "deps";

    /// Builds the runtime shim library and returns the path to the target output
    /// directory, which holds the artifacts and dependencies.
    pub(super) fn build_lib(proj_path: &Path) -> PathBuf {
        const SHIM_LIB_TARGET_DIR: &str = "target/compiler";

        // Use the same profile as the compiler
        let profile = env::var("PROFILE").unwrap();

        Command::new(env::var("CARGO").unwrap())
            .arg("build")
            .args(["--target-dir", SHIM_LIB_TARGET_DIR])
            .args([
                "--profile",
                if profile == "debug" { "dev" } else { &profile },
            ])
            .current_dir(proj_path)
            .spawn()
            .and_then(|mut c| c.wait())
            .expect("Cargo build did not start for the shim lib.")
            .exit_ok()
            .expect("Building the shim lib failed.");
        proj_path.join(SHIM_LIB_TARGET_DIR).join(profile)
    }

    /// Copies the built artifact and dependencies of the runtime shim to the
    /// the compiler's `deps` directory in a dedicated folder.
    pub(super) fn copy_lib_to_deps(lib_output_dir: &Path) -> PathBuf {
        let deps_dir = artifacts_path().join(DIR_DEPS);
        let shim_lib_copy_dir = deps_dir.join("runtime_shim");
        recreate_dir(&shim_lib_copy_dir);
        copy_built_files(lib_output_dir, &shim_lib_copy_dir);
        shim_lib_copy_dir
    }

    /// Copies the built artifact and dependencies of the runtime shim.
    fn copy_built_files(lib_output_dir: &Path, dst_dir: &Path) {
        assert!(lib_output_dir.is_dir());
        assert!(dst_dir.is_dir());

        use fs::copy;
        println!("{:?} -> {:?}", lib_output_dir, dst_dir);
        copy(
            lib_output_dir.join(SHIM_LIB_FILE_NAME),
            dst_dir.join(SHIM_LIB_FILE_NAME),
        )
        .unwrap();

        let dst_deps_dir = dst_dir.join(DIR_DEPS);
        recreate_dir(&dst_deps_dir);
        for entry_result in lib_output_dir.join(DIR_DEPS).read_dir().unwrap() {
            let Ok(entry) = entry_result else { continue };
            if !entry
                .file_type()
                .is_ok_and(|t| t.is_file() || t.is_symlink())
            {
                continue;
            }

            copy(entry.path(), dst_deps_dir.join(entry.file_name())).unwrap();
        }
    }
}

fn provide_toolchain_builder(workspace_dir: &Path) {
    let script_path = workspace_dir.join(PathBuf::from_iter(PATH_TOOLCHAIN_BUILDER));
    println!("cargo:rerun-if-changed={}", script_path.display());
    let link_path = artifacts_path().join(FILE_TOOLCHAIN_BUILDER);
    if link_path.exists() {
        return;
    }
    #[cfg(unix)]
    std::os::unix::fs::symlink(script_path, link_path)
        .expect("Could not create symlink to the toolchain builder script.");
}

fn recreate_dir(dir: &Path) {
    if dir.exists() {
        fs::remove_dir_all(dir).unwrap_or_else(|_| panic!("Failed to remove {:?}", dir));
    }
    fs::create_dir_all(dir).unwrap_or_else(|_| panic!("Failed to create {:?}", dir));
}

fn artifacts_path() -> PathBuf {
    /* At the moment, no cleaner way is found to get the artifacts path.
     * This is based on the default structure: target/profile/build/<crate>/out */
    PathBuf::from(env::var("OUT_DIR").unwrap())
        .ancestors()
        .skip(3) // profile/build/<crate>/out -> profile
        .next()
        .unwrap()
        .to_path_buf()
}
