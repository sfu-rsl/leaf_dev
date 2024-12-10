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

pub(super) fn build_toolchain(
    sysroot: &Path,
    crate_out_dir: Option<&Path>,
) -> Result<PathBuf, String> {
    let builder = try_find_dependency_path(PathBuf::from_iter(PATH_TOOLCHAIN_BUILDER), [])
        .ok_or("Failed to find the toolchain builder".to_owned())?;

    let (work_dir, out_dir) = setup_work_and_out_dirs(crate_out_dir)?;

    let mut cmd = process::Command::new(&builder);
    set_env_vars(&mut cmd, &work_dir, &out_dir, sysroot)?;
    let log_filename = set_log_file(&mut cmd, &work_dir)?;
    cmd.current_dir(&work_dir);

    let built_toolchain_path = run_builder(cmd).map_err(|e| {
        format!(
            "{e}. You can check the log file at: {}",
            log_filename.display()
        )
    })?;
    log_debug!("Toolchain built at: {}", built_toolchain_path.display());

    log_debug!("Deleting the work directory");
    let _ = fs::remove_dir_all(&work_dir)
        .inspect_err(|e| log_warn!("Could not delete the work directory: {}", e));

    let cache_path = cache_toolchain(&built_toolchain_path, sysroot)?;

    log_debug!("Toolchain is now available at: {}", cache_path.display());
    Ok(cache_path)
}

fn setup_work_and_out_dirs(crate_out_dir: Option<&Path>) -> Result<(PathBuf, PathBuf), String> {
    fn create_new_dir(path: &Path) -> Result<(), String> {
        if path.exists() {
            let _ = fs::remove_dir_all(&path).inspect_err(|e| {
                log_warn!("Could not delete the existing work directory: {}", e);
            });
        }
        fs::create_dir_all(&path)
            .map_err(|e| format!("Failed to create a work directory: {}", e))?;
        log_debug!("Created a work directory: {}", path.display());
        Ok(())
    }

    let id = current_instant();
    let work_dir = crate_out_dir
        .map(Path::to_path_buf)
        .unwrap_or_else(env::temp_dir)
        .join(DIR_LEAFC_WORK)
        .join(DIR_TOOLCHAIN_BUILDER_WORK)
        .join(&id);
    create_new_dir(&work_dir)?;

    let out_dir = env::current_exe()
        .map_err(|e| format!("Failed to get the current executable path: {}", e))?
        .parent()
        .unwrap()
        .join(format!("{DIR_TOOLCHAIN}_{id}"));
    create_new_dir(&out_dir)?;

    Ok((work_dir, out_dir))
}

fn set_log_file(cmd: &mut process::Command, work_dir: &Path) -> Result<PathBuf, String> {
    let filename = work_dir.join(format!("log_{}.log", current_instant()));
    let file = fs::File::create_new(&filename)
        .map_err(|e| format!("Failed to create the toolchain builder stderr file: {}", e))?;

    cmd.stderr(process::Stdio::from(file));

    Ok(filename)
}

fn set_env_vars(
    cmd: &mut process::Command,
    work_dir: &Path,
    out_dir: &Path,
    sysroot: &Path,
) -> Result<(), String> {
    let exe_path = env::current_exe()
        .map_err(|e| format!("Failed to get the current executable path: {}", e))?;

    // FIXME: Change these to arguments.
    cmd.env(ENV_WORK_DIR, work_dir)
        .env(ENV_OUT_DIR, out_dir)
        .env(ENV_LEAFC, exe_path)
        .env(ENV_TOOLCHAIN_MARKER, FILE_TOOLCHAIN_MARKER)
        .env(ENV_RUSTUP_TOOLCHAIN, sysroot)
        .env(ENV_LEAF_WORKSPACE, PATH_WORKSPACE);

    Ok(())
}

fn run_builder(mut cmd: process::Command) -> Result<PathBuf, String> {
    cmd.stdout(process::Stdio::piped())
        .stdin(process::Stdio::null());

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

fn cache_toolchain(built_toolchain_path: &Path, sysroot: &Path) -> Result<PathBuf, String> {
    log_debug!("Moving the built toolchain next to the compiler");
    let cache_path = env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .join(DIR_TOOLCHAIN);
    if cache_path.exists() {
        // A naive approach to prevent concurrency conflicts.
        if is_sysroot_compatible(sysroot, Some(&cache_path)) {
            log_debug!("Toolchain already exists at: {}", cache_path.display());
            fs::remove_dir_all(&built_toolchain_path).map_err(|e| {
                format!(
                    "Could not delete the unused built toolchain directory: {}",
                    e
                )
            })?;
            return Ok(cache_path);
        } else {
            log_debug!("Deleting the existing cached toolchain directory");
            fs::remove_dir_all(&cache_path)
                .map_err(|e| format!("Could not delete the existing toolchain directory: {}", e))?;
        }
    }

    fs::rename(&built_toolchain_path, &cache_path).map_err(|e| format!("Failed to move: {}", e))?;

    Ok(cache_path)
}

pub(super) fn is_sysroot_compatible(sysroot: &Path, built_sysroot: Option<&Path>) -> bool {
    // If the original set sysroot has the marker, it is our own sysroot.
    if sysroot.join(FILE_TOOLCHAIN_MARKER).exists() {
        return true;
    }

    let Some(built_sysroot) = built_sysroot else {
        return false;
    };

    let original_sysroot = match fs::read_to_string(built_sysroot.join(FILE_TOOLCHAIN_MARKER)) {
        Ok(original_sysroot) => PathBuf::from(original_sysroot.trim()),
        Err(e) => {
            log_warn!("Failed to read the toolchain marker file: {}", e);
            return false;
        }
    };

    Option::zip(
        fs::canonicalize(sysroot).ok(),
        fs::canonicalize(original_sysroot).ok(),
    )
    .is_some_and(|(a, b)| a == b)
}

fn current_instant() -> String {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis()
        .to_string()
}
