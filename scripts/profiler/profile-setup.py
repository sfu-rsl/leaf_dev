import os
import subprocess
import sys
import logging
import logging.config
from typing import List, Callable
import uuid
from pathlib import Path

DIR_RUNTIME_LIB = ("runtime", "lib")
ENV_LOG_LEVEL = "PROF_LOG_LEVEL"
ENV_LEAF_WORKSPACE = "LEAF_WORKSPACE"
SKIP_MACRO = "trait_log_fn"
IS_STASHED = False


def run_command(command, check=True, capture_output=True, text=True, cwd=None, env=None):
    """
    Run a command and handle exceptions.

    Args:
    command (List[str]): The command to run.
    check (bool): If True, raise a CalledProcessError for non-zero exit codes.
    capture_output (bool): If True, capture stdout and stderr.
    text (bool): If True, return strings instead of bytes for stdout and stderr.
    cwd (str): The working directory for the command.
    env (dict): Environment variables for the command.

    Returns:
    tuple: (success, output, error)
    """
    try:
        result = subprocess.run(
            command, check=check, capture_output=capture_output, text=text, cwd=cwd, env=env)
        return True, result.stdout, result.stderr
    except subprocess.CalledProcessError as e:
        logging.error(f"Error executing command {' '.join(command)}: {e}")
        return False, e.stdout, e.stderr


def find_leaf_workspace_dir() -> Path:
    workspace_dir = os.environ.get(ENV_LEAF_WORKSPACE)
    if not workspace_dir:
        current_dir = Path(__file__).parent
        while not current_dir.joinpath("Cargo.toml").exists():
            parent = current_dir.parent
            if parent == current_dir:
                raise FileNotFoundError("Leaf workspace not found")
            current_dir = parent
        workspace_dir = current_dir

    assert workspace_dir.joinpath("common").exists()
    assert workspace_dir.joinpath("runtime").exists()

    return workspace_dir


def run_cargo_fmt(root):
    logging.debug("Running 'cargo fmt' to format Rust code")
    success, output, error = run_command(["cargo", "fmt"], cwd=root)
    if success:
        logging.debug("Cargo fmt output:")
        logging.debug(output)
    return success


def check_uncommitted_changes():
    success, output, _ = run_command(["git", "status", "--porcelain"])
    if success:
        if not output.strip():
            logging.debug("No outstanding changes in directory")
            return True
        else:
            logging.error("Uncommitted/ Outstanding changes in directory")
            return False
    return False


def add_changes_to_git():
    logging.debug("Adding changes to git")
    success, _, _ = run_command(["git", "add", "-A"])
    return success


def check_for_clippy_tracing():
    logging.debug("checking if clippy_tracing is installed")
    success, _, _ = run_command(["cargo", "install", "clippy-tracing"])
    return success


def checkout_new_branch():
    branch_name = f"automatic-profiler-branch-{uuid.uuid4().hex[:8]}"
    logging.debug(f"Creating and checking out new branch: {branch_name}")
    success, _, _ = run_command(["git", "checkout", "-b", branch_name])
    if success:
        logging.debug(
            f"Checked out to new branch '{branch_name}' successfully.")
    return success


def stash_changes():
    if check_uncommitted_changes():
        return True
    logging.debug("Stashing changes")
    success, _, _ = run_command(["git", "stash", "--include-untracked"])
    if success:
        logging.debug("Changes stashed successfully.")
        set_stashed()
    return success


def stash_apply():
    logging.debug("Applying stashed changes")
    global IS_STASHED
    if not IS_STASHED:
        logging.debug("No Stashed changes Applied")
        return True
    success, _, _ = run_command(["git", "stash", "pop"])
    if success:
        logging.debug("Stashed changes applied successfully.")
    return success


def checkout_new_branch():
    """Checkout a new branch with a UID name and commit the changes."""
    # Generate a unique branch name using UUID
    branch_name = f"automatic-profiler-branch-{uuid.uuid4().hex[:8]}"
    logging.debug(f"Creating and checking out new branch: {branch_name}")

    # Checkout a new branch with the generated name
    success, _, _ = run_command(["git", "checkout", "-b", branch_name])
    if success:
        logging.debug(
            f"Checked out to new branch '{branch_name}' successfully.")
        return success


def commit_changes():
    # if no uncomitted changes skip committing
    if check_uncommitted_changes():
        return True
    logging.debug("Committing changes as 'automated-profiler-commit'")
    success, _, _ = run_command(
        ["git", "commit", "-a", "-m", "automated-profiler-commit"])
    if success:
        logging.debug("Profiler setup changes committed successfully.")
    return success


def instrument_runtime_lib(workspace_dir: Path):
    def process_rust_files(directory: str):
        command = ["clippy-tracing", "--action", "fix", "--path", "{file}"]
        for root, _, files in os.walk(directory):
            for file in files:
                if file.endswith(".rs"):
                    file_path = os.path.join(root, file)
                    if should_skip_file(file_path):
                        print(f"{file_path} skipping")
                        continue
                    current_command = [arg.replace(
                        "{file}", file_path) for arg in command]
                    print(current_command)
                    print(file)
                    success, output, _ = run_command(current_command)
                    if success:
                        print(f"Processed {file_path}")
                        print(output)
        return True

    runtime_dir = workspace_dir.joinpath(*DIR_RUNTIME_LIB)
    return process_rust_files(str(runtime_dir))


def execute_sample_project(workspace_dir: Path):
    logging.debug("building sample program")
    EXE_NAME = "sample"

    def compile_sample_project():
        logging.debug("compiling sample program")
        logging.debug("Running Cargo command...")
        success, output, _ = run_command([
            "cargo",
            "run",
            "--manifest-path",
            str(workspace_dir.joinpath("Cargo.toml")),
            "--features",
            "profile_flame",
            str(workspace_dir.joinpath("samples",
                "branching", "if_basic", "main.rs")),
            "-o",
            EXE_NAME,
        ])
        if success:
            logging.debug("Command output:")
            logging.debug(output)
        return success

    compile_sample_project()
    env = os.environ.copy()
    env["LEAF_LOG"] = "trace,pri=trace,z3=trace"

    exe_path = os.path.join(os.getcwd(), EXE_NAME)
    success, output, _ = run_command([exe_path], env=env)
    try:
        os.remove(exe_path)
        logging.debug("executable removed")
    except Exception as e:
        print(f"Error deleting file '{exe_path}': {e}")

    return success


def should_skip_file(file_name):
    with open(file_name, "r") as file:
        file_content = file.read()
    return SKIP_MACRO in file_content


def set_stashed():
    global IS_STASHED
    IS_STASHED = True


def main():
    workspace_dir = find_leaf_workspace_dir()
    print(workspace_dir)

    if not check_for_clippy_tracing():
        logging.error("Failed to install clippy-tracing")
        return

    if not stash_changes():
        logging.error("Failed to stash changes")
        return

    if not checkout_new_branch():
        logging.error("Failed to checkout to a new branch")
        return

    if not instrument_runtime_lib(workspace_dir):
        logging.error("Failed to instrument runtime lib")
        return

    if not run_cargo_fmt(workspace_dir):
        logging.error("Failed to run cargo fmt")
        return

    if not (add_changes_to_git() and commit_changes()):
        logging.error("Failed to add and commit changes")
        return

    if not stash_apply():
        logging.error("Failed to apply stashed changes")
        return

    if not execute_sample_project(workspace_dir):
        logging.error("Failed to execute sample project")
        return

    logging.info("Sample project compiled and executed successfully")


if __name__ == "__main__":
    logging.basicConfig(
        level=os.environ.get(ENV_LOG_LEVEL, default="DEBUG").upper(),
        format="[%(asctime)s %(levelname)s] %(message)s",
    )
    main()
