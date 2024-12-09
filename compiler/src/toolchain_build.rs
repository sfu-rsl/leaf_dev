use std::{
    env, fs,
    io::BufRead,
    path::{Path, PathBuf},
    process,
};

use common::{log_debug, log_warn};
use const_format::concatcp;

use crate::utils::file::try_find_dependency_path;

use super::constants::{DIR_TOOLCHAIN, ENV_RUSTUP_TOOLCHAIN};

const PATH_TOOLCHAIN_BUILDER: [&str; 2] = ["toolchain_builder", "build"];

const DIR_LEAFC_WORK: &str = "leafc";
const DIR_TOOLCHAIN_BUILDER_WORK: &str = "toolchain_builder";

const PREFIX_LEAF_SCRIPT_CONFIG: &str = "LEAFS";
const ENV_WORK_DIR: &str = "WORK_DIR";
const ENV_OUT_DIR: &str = "OUT_DIR";
const ENV_LEAFC: &str = "LEAFC";
const ENV_TOOLCHAIN_MARKER: &str = concatcp!(PREFIX_LEAF_SCRIPT_CONFIG, "_TOOLCHAIN_MARKER_FILE");
const ENV_LEAF_WORKSPACE: &str = "LEAF_WORKSPACE";

pub const FILE_TOOLCHAIN_MARKER: &str = ".leafc_toolchain";

const PATH_WORKSPACE: &str = env!("WORKSPACE_DIR"); // Set by the build script.

pub(super) fn build_toolchain(sysroot: &Path, out_dir: Option<&Path>) -> Result<PathBuf, String> {
    let builder = try_find_dependency_path(PathBuf::from_iter(PATH_TOOLCHAIN_BUILDER), [])
        .ok_or("Failed to find the toolchain builder".to_owned())?;

    let work_dir = setup_workdir(out_dir)?;

    let mut cmd = process::Command::new(&builder);
    set_env_vars(&mut cmd, &work_dir, sysroot)?;
    let log_filename = set_log_file(&mut cmd, &work_dir)?;
    cmd.current_dir(&work_dir);

    let toolchain_path = run_builder(cmd).map_err(|e| {
        format!(
            "{e}. You can check the log file at: {}",
            log_filename.display()
        )
    })?;

    log_debug!("Deleting the work directory");
    let _ = fs::remove_dir_all(&work_dir)
        .inspect_err(|e| log_warn!("Could not delete the work directory: {}", e));

    let target_dir = toolchain_path.with_file_name(DIR_TOOLCHAIN);
    fs::rename(&toolchain_path, &target_dir)
        .map_err(|e| format!("Failed to rename the toolchain directory: {}", e))?;

    log_debug!("Toolchain built at: {}", target_dir.display());

    Ok(target_dir)
}

fn setup_workdir(out_dir: Option<&Path>) -> Result<PathBuf, String> {
    let work_dir = out_dir
        .map(Path::to_path_buf)
        .unwrap_or_else(env::temp_dir)
        .join(DIR_LEAFC_WORK)
        .join(DIR_TOOLCHAIN_BUILDER_WORK);

    if work_dir.exists() {
        let _ = fs::remove_dir_all(&work_dir).inspect_err(|e| {
            log_warn!("Could not delete the existing work directory: {}", e);
        });
    }
    fs::create_dir_all(&work_dir)
        .map_err(|e| format!("Failed to create the work directory: {}", e))?;

    log_debug!("Created the work directory: {}", work_dir.display());

    Ok(work_dir)
}

fn set_log_file(cmd: &mut process::Command, work_dir: &Path) -> Result<PathBuf, String> {
    let filename = work_dir.join(format!(
        "log_{}.log",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis()
    ));
    let file = fs::File::create_new(&filename)
        .map_err(|e| format!("Failed to create the toolchain builder stderr file: {}", e))?;

    cmd.stderr(process::Stdio::from(file));

    Ok(filename)
}

fn set_env_vars(
    cmd: &mut process::Command,
    work_dir: &PathBuf,
    sysroot: &Path,
) -> Result<(), String> {
    let exe_path = env::current_exe()
        .map_err(|e| format!("Failed to get the current executable path: {}", e))?;

    // FIXME: Change these to arguments.
    cmd.env(ENV_WORK_DIR, work_dir)
        .env(ENV_OUT_DIR, exe_path.parent().unwrap())
        .env(ENV_LEAFC, exe_path)
        .env(ENV_TOOLCHAIN_MARKER, FILE_TOOLCHAIN_MARKER)
        .env(ENV_RUSTUP_TOOLCHAIN, sysroot)
        .env(ENV_LEAF_WORKSPACE, PATH_WORKSPACE);

    Ok(())
}

fn run_builder(mut cmd: process::Command) -> Result<PathBuf, String> {
    cmd.stdout(process::Stdio::piped());

    log_debug!("Running the toolchain builder");

    let child = cmd
        .spawn()
        .map_err(|e| format!("Failed to execute the toolchain builder: {}", e))?;

    let stdout = child
        .wait_with_output()
        .map_err(|e| format!("Failed to finish execution of the toolchain builder: {}", e))
        .and_then(|result| {
            result
                .status
                .success()
                .then_some(result.stdout)
                .ok_or(format!(
                    "Toolchain builder exited with status: {}",
                    result.status,
                ))
        })?;

    let toolchain_path = stdout
        .lines()
        .last()
        .ok_or("Failed to read the toolchain path")?
        .map_err(|e| format!("Failed to read the toolchain path: {}", e))?;
    Ok(toolchain_path.into())
}

pub(super) fn is_sysroot_compatible(sysroot: &Path) -> bool {
    sysroot.join(FILE_TOOLCHAIN_MARKER).exists()
}
