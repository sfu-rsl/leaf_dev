from dataclasses import dataclass
import logging.config
import subprocess
import os
import shutil
from pathlib import Path
import logging

ENV_WORK_DIR = "WORK_DIR"
ENV_OUT_DIR = "OUT_DIR"
ENV_LEAF_WORKSPACE = "LEAF_WORKSPACE"
ENV_LOG_LEVEL = "LEAFS_LOG_LEVEL"

ENV_LEAFC = "LEAFC"
ENV_CARGO = "CARGO"
ENV_RUSTC = "RUSTC"

BUILD_TARGET = "x86_64-unknown-linux-gnu"
# debug or release
BUILD_PROFILE = "release"

SHIM_MODULE_NAME = "leafrtsh"


@dataclass
class Paths:
    work: Path
    out: Path
    res: Path
    leaf_workspace: Path


def ensure_env_dir(var_name: str, default) -> Path:
    path = Path(os.environ.get(var_name, default=default))
    assert not path.exists() or path.is_dir()
    path.mkdir(parents=True, exist_ok=True)
    return path


def find_leaf_workspace_dir() -> Path:
    workspace_dir = os.environ.get(ENV_LEAF_WORKSPACE)
    if not workspace_dir:
        # Traverse up the parent directories to find the workspace
        current_dir = Path(__file__).parent
        while not current_dir.joinpath("Cargo.toml").exists():
            parent = current_dir.parent
            if parent == current_dir:
                raise FileNotFoundError("Leaf workspace not found")
            current_dir = parent
        workspace_dir = current_dir

    # Check some assumed structure
    assert workspace_dir.joinpath("common").exists()
    assert workspace_dir.joinpath("runtime").exists()

    return workspace_dir


def get_toolchain_path(cwd, env=None) -> Path:
    try:
        process = subprocess.run(
            args=["cargo", "rustc", "--", "--print=sysroot"],
            cwd=cwd,
            env=env,
            check=True,
            capture_output=True,
        )
        return Path(process.stdout.decode("utf-8").strip())
    except subprocess.CalledProcessError as e:
        logging.error("Failed to get the toolchain path: %s", e.stderr.decode("utf-8"))
        raise


def get_core_src_dir(toolchain_path) -> Path:
    return toolchain_path.joinpath(
        "lib", "rustlib", "src", "rust", "library", "core", "src"
    )


def copy_toolchain(toolchain_path: Path, work_dir: Path) -> Path:
    dst_dir = work_dir.joinpath("toolchain")
    if dst_dir.exists():
        shutil.rmtree(dst_dir)
    dst_dir.mkdir(parents=True)

    core_src_dir = get_core_src_dir(toolchain_path)
    assert (
        core_src_dir.exists()
    ), "Core source directory does not exist. Make sure you have added the source component to the toolchain."

    # Symbolically copy the toolchain to the work directory except for the core source
    work_list = [toolchain_path]
    while len(work_list) > 0:
        current = work_list.pop()
        for child in current.iterdir():
            if core_src_dir.is_relative_to(child):
                if child == core_src_dir:
                    # Copy the actual core source
                    shutil.copytree(
                        child,
                        dst_dir.joinpath(child.relative_to(toolchain_path)),
                        dirs_exist_ok=True,
                    )
                else:
                    work_list.append(child)
            else:
                # Linking the rest
                link_path = dst_dir.joinpath(child.relative_to(toolchain_path))
                link_path.parent.mkdir(parents=True, exist_ok=True)

                # Binary files must have a real path to give the correct sysroot.
                # The rest like source files can be symbolically linked.
                if "src" in str(link_path):
                    link_path.symlink_to(child, target_is_directory=True)
                elif child.is_dir():
                    shutil.copytree(
                        child,
                        link_path,
                        copy_function=os.link,
                    )
                else:
                    link_path.hardlink_to(child)

    return dst_dir


def add_leaf_to_core(core_src_dir: Path, leaf_workspace_dir: Path, res_dir: Path):
    module_dir = core_src_dir.joinpath("leaf")
    module_dir.mkdir(exist_ok=True)

    def add_common_module(common_src_dir: Path):
        logging.debug("Adding common")
        dst_dir = module_dir.joinpath("common")
        dst_dir.mkdir(exist_ok=True)
        modules = ["ffi", "pri", "types", "utils"]
        for module in modules:
            shutil.copy(common_src_dir.joinpath(f"{module}.rs"), dst_dir)
        # Making appropriate mod.rs
        with open(dst_dir.joinpath("mod.rs"), "w") as f:
            f.writelines((f"pub(crate) mod {m};" for m in modules))

    def add_shim_module(shim_src_dir: Path):
        logging.debug("Adding the runtime shim library")
        dst_dir = module_dir.joinpath(SHIM_MODULE_NAME)
        shutil.copytree(shim_src_dir, dst_dir, dirs_exist_ok=True)
        # Rename lib.rs to mod.rs
        shutil.move(dst_dir.joinpath("lib.rs"), dst_dir.joinpath("mod.rs"))

    def apply_patches():
        logging.debug("Applying patches")
        for patch_path in res_dir.joinpath("patches").glob("*.patch"):
            subprocess.run(
                args=[
                    "git",
                    "apply",
                    "-C1",
                    "--unsafe-paths",
                    "--directory",
                    core_src_dir,
                    patch_path,
                ],
                cwd=core_src_dir,
                check=True,
            )
        # Probe patching is effective.
        assert Path(core_src_dir.joinpath("leaf", "mod.rs")).exists()

    logging.info("Adding leaf to the core library source")

    add_common_module(leaf_workspace_dir.joinpath("common", "src"))
    add_shim_module(leaf_workspace_dir.joinpath("runtime", "shim", "src"))
    apply_patches()


def create_dummy_crate(work_dir: Path, res_dir: Path) -> Path:
    template_dir = res_dir.joinpath("crate_template")
    crate_path = work_dir.joinpath("dummy_crate")
    if crate_path.exists():
        shutil.rmtree(crate_path)
    shutil.copytree(template_dir, crate_path)

    return crate_path


def substitute_template(template_path: Path, **substitutions: object):
    from string import Template

    with open(template_path, "r") as f:
        template = Template(f.read())

    with open(template_path.with_suffix(""), "w") as f:
        f.write(template.substitute(**substitutions))


def set_dummy_crate_toolchain(dummy_crate_dir: Path, toolchain_path: Path):
    substitute_template(
        dummy_crate_dir.joinpath(".cargo", "config.toml.template"),
        toolchain_path=toolchain_path.absolute(),
    )


def get_build_env(toolchain_path: Path):
    return {
        **os.environ,
        ENV_RUSTC: os.environ.get(ENV_LEAFC, default="leafc"),
        "RUSTUP_TOOLCHAIN": str(toolchain_path),
    }


def build_crate_with_core(dummy_crate_dir: Path, toolchain_path):
    logging.info("Building the core library through dummy crate")

    output = subprocess.DEVNULL if logging.getLogger().level > logging.DEBUG else None
    args = [
        "cargo",
        "build",
        # Causes the core lib to be built as a dependency
        "-Zbuild-std=core",
        f"--target={BUILD_TARGET}",
        "--verbose",
        f"--profile={'dev' if BUILD_PROFILE == 'debug' else BUILD_PROFILE}",
    ]
    logging.debug("Running: %s", " ".join(args))
    subprocess.run(
        args=args,
        cwd=dummy_crate_dir,
        env=get_build_env(toolchain_path),
        check=True,
        stdout=output,
        stderr=output,
    )

    logging.info("Successfully built the core library.")


def make_toolchain(dummy_crate_dir: Path, out_dir: Path) -> Path:
    logging.info("Making a toolchain from the built libraries")

    deps_dir = dummy_crate_dir.joinpath("target", BUILD_TARGET, BUILD_PROFILE, "deps")
    # As this dummy crate depends only on the core lib, all of the dependencies
    # are part of the sysroot.
    logging.debug("Globbing lib files in %s", deps_dir)
    lib_files = set(deps_dir.glob("lib*.r*")) - set(deps_dir.glob("libdummy*"))
    logging.debug("Found lib files: %s", lib_files)

    toolchain_dir = out_dir.joinpath("toolchain")
    dst_dir = toolchain_dir.joinpath("lib", "rustlib", BUILD_TARGET, "lib")
    dst_dir.mkdir(parents=True, exist_ok=True)
    # Clear the destination directory
    for file in dst_dir.glob("lib*.r*"):
        file.unlink()
    for file in lib_files:
        shutil.copy(file, dst_dir)

    return toolchain_dir


def main():
    logging.info("Building core lib with leaf")

    paths = Paths(
        work=ensure_env_dir(ENV_WORK_DIR, default=Path("./work")),
        out=ensure_env_dir(ENV_OUT_DIR, default=Path("./out")),
        res=Path(__file__).parent.joinpath(f"res"),
        leaf_workspace=find_leaf_workspace_dir(),
    )
    logging.debug("Paths: %s", paths.__repr__())

    dummy_crate_dir = create_dummy_crate(paths.work, paths.res)

    orig_toolchain_path = get_toolchain_path(cwd=dummy_crate_dir, env=os.environ)
    logging.debug("Original toolchain path: %s", orig_toolchain_path)
    copied_toolchain_path = copy_toolchain(orig_toolchain_path, paths.work)
    logging.debug("Copied toolchain path: %s", copied_toolchain_path)

    set_dummy_crate_toolchain(dummy_crate_dir, copied_toolchain_path)
    toolchain_path = get_toolchain_path(
        cwd=dummy_crate_dir, env=get_build_env(copied_toolchain_path)
    )
    logging.debug("Toolchain path: %s", toolchain_path)
    assert toolchain_path.absolute() == copied_toolchain_path.absolute(), toolchain_path

    core_src = get_core_src_dir(toolchain_path)
    logging.debug("Core source dir: %s", core_src)
    add_leaf_to_core(core_src, paths.leaf_workspace, paths.res)

    build_crate_with_core(dummy_crate_dir, toolchain_path)
    out_toolchain_path = make_toolchain(dummy_crate_dir, paths.out)

    logging.info(
        "The toolchain is available at %s",
        out_toolchain_path,
    )


if __name__ == "__main__":
    logging.basicConfig(
        level=os.environ.get(ENV_LOG_LEVEL, default="INFO").upper(),
        format="[%(asctime)s %(levelname)s] %(message)s",
    )
    main()
