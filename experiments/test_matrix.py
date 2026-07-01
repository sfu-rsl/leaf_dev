#!/usr/bin/env python3
"""Run `cargo test` for Rust crates with leafc across shared TOML configurations."""

from __future__ import annotations

import argparse
import glob
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence
from string import Template


@dataclass(frozen=True)
class TestSpec:
    crate_dir: Path
    config_path: Path
    out_dir: Path
    target_args: Sequence[str]
    doc: bool = False
    profile: str = "release"
    package_name: str | None = None
    leafc_path: str = "leafc"
    cargo_path: str = "cargo"


def list_config_files(configs_glob: str) -> list[Path]:
    configs = sorted(
        Path(p).absolute()
        for p in glob.glob(configs_glob, recursive=True)
        if Path(p).is_file() and p.endswith(".toml")
    )
    if not configs:
        raise FileNotFoundError(f"No TOML files found for pattern: {configs_glob}")
    print(f"Found {len(configs)} config files: {configs}")
    return configs


def load_config_text(config_path: Path, package_name: str, out_dir: Path) -> str:
    text = config_path.read_text(encoding="utf-8")
    if config_path.name.endswith(".template.toml"):
        substitutions = {"CRATE_NAME": package_name.replace("-", "_")}
        template = Template(text)
        text = template.substitute(substitutions)
        out_dir.mkdir(parents=True, exist_ok=True)

    appendix_path = config_path.with_name("appendix.toml.ignore")
    print(f"Checking for appendix file: {appendix_path}")
    if appendix_path.is_file():
        appendix_text = appendix_path.read_text(encoding="utf-8")
        if text and not text.endswith("\n"):
            text += "\n"
        text += appendix_text

    config_file = out_dir / "config.toml"
    config_file.write_text(text, encoding="utf-8")
    return text


def build_command(spec: TestSpec, target_args: Sequence[str] = None) -> list[str]:
    manifest = spec.crate_dir / "Cargo.toml"
    if not manifest.is_file():
        raise FileNotFoundError(
            f"Missing Cargo.toml in crate directory: {spec.crate_dir}"
        )

    cmd = [
        spec.cargo_path,
        "test",
        "--manifest-path",
        str(manifest),
        "--target-dir",
        str(spec.out_dir),
        "--profile",
        spec.profile,
        "--verbose",
        "--timings",
        "--message-format",
        "json",
        # "--jobs",
        # "1",
    ]

    if target_args is None:
        target_args = spec.target_args
    if target_args:
        cmd.extend(target_args)

    try:
        harness_args_start = cmd.index("--") + 1
    except ValueError:
        cmd.append("--")
        harness_args_start = len(cmd)

    cmd[harness_args_start:harness_args_start] = ["--test-threads=1", "--test", "-Zunstable-options", "--ensure-time"]

    return cmd


def build_env(config_toml_text: str, leafc_path: str, shared_lib_dir: Path) -> dict[str, str]:
    env = dict(os.environ)
    env.pop("RUSTC", None)
    env.pop("RUSTFLAGS", None)
    env.pop("RUSTDOC", None)
    env.pop("RUSTDOCFLAGS", None)
    env.pop("LEAFC_CONFIG_STR", None)
    env.pop("LEAFC_CONFIG_STR_FMT", None)
    env.pop("LEAFC_LOG", None)

    env["RUSTC"] = leafc_path
    sysroot = subprocess.run(
        [leafc_path, "--print", "sysroot"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()
    env["RUSTDOCFLAGS"] = f"-Zunstable-options --test-builder={leafc_path} --sysroot {sysroot}"
    env["RUSTFLAGS"] = "-Zverify-llvm-ir"
    env["LEAFC_CONFIG_STR"] = config_toml_text
    env["LEAFC_CONFIG_STR_FMT"] = "toml"
    env["CARGO_INCREMENTAL"] = "false"
    # env["LEAFC_LOG"] = "info"
    # env["RUSTC_LOG"] = "info"
    env["RUST_BACKTRACE"] = "full"
    env["RUSTC_BACKTRACE"] = "full"
    env["RUST_TEST_TIME_UNIT"] = "120000,120000"
    env["RUST_TEST_TIME_INTEGRATION"] = "120000,120000"
    env["RUST_TEST_TIME_DOCTEST"] = "120000,120000"


    env["LD_LIBRARY_PATH"] = str(shared_lib_dir) + os.pathsep + env.get("LD_LIBRARY_PATH", "")

    return env


def has_existing_result(spec: TestSpec) -> bool:
    profile_out_dir = spec.out_dir / (
        spec.profile if spec.profile != "dev" else "debug"
    )
    return profile_out_dir.is_dir() and (spec.out_dir / "test.log").is_file()


def run_test(spec: TestSpec, shared_lib_dir: Path, dry_run: bool = False, force: bool = False) -> int:
    if has_existing_result(spec):
        if not force:
            print(
                f"\n[skip] crate={spec.crate_dir} config={spec.config_path.name} out={spec.out_dir}"
            )
            print("       existing artifacts detected; use --force to re-run tests")
            return 0

        print(
            f"\n[force] removing existing output for crate={spec.crate_dir} config={spec.config_path.name}"
        )
        print(f"       rm -rf {spec.out_dir}")
        if not dry_run:
            shutil.rmtree(spec.out_dir)

    spec.out_dir.mkdir(parents=True, exist_ok=True)
    output_log = spec.out_dir / "test.log"
    package_name = spec.package_name or spec.crate_dir.name
    config_text = load_config_text(spec.config_path, package_name, spec.out_dir)
    env = build_env(config_text, spec.leafc_path, shared_lib_dir)
    cmd = build_command(spec)

    print(
        f"\n[test] crate={spec.crate_dir} config={spec.config_path.name} out={spec.out_dir}"
    )
    print("       ", " ".join(cmd))
    print(f"       output -> {output_log}")
    if spec.doc:
        print("+     [doc tests]")

    if dry_run:
        return 0

    with output_log.open("w", encoding="utf-8") as log_file:
        log_file.write("$ " + " ".join(cmd) + "\n\n")
        log_file.flush()
        completed = subprocess.run(
            cmd,
            env=env,
            cwd=spec.out_dir,
            stdout=log_file,
            stderr=subprocess.STDOUT,
            text=True,
        )
        returncode = completed.returncode
        if returncode != 0:
            return returncode
        
        if spec.doc:
            cmd = build_command(spec, target_args=["--doc"])
            log_file.write("$ " + " ".join(cmd) + "\n\n")
            log_file.flush()
            completed_doc = subprocess.run(
                cmd,
                env=env,
                cwd=spec.out_dir,
                stdout=log_file,
                stderr=subprocess.STDOUT,
                text=True,
            )
            returncode = completed_doc.returncode


    return returncode


def run_test_matrix(
    crate: Path,
    configs: Sequence[Path],
    output_root: Path,
    target_args: Sequence[str],
    doc: bool,
    profile: str,
    package_name: str | None,
    leafc_path: str,
    shared_lib_dir: Path,
    cargo_path: str,
    dry_run: bool,
    force: bool,
) -> int:
    exit_code = 0
    crate_name = crate.resolve().name
    for config in configs:
        out_dir = (
            output_root / config.stem.replace(".template", "") / crate_name / profile
        )
        spec = TestSpec(
            crate_dir=crate,
            config_path=config,
            out_dir=out_dir,
            target_args=target_args,
            doc=doc,
            profile=profile,
            package_name=package_name or crate_name,
            leafc_path=leafc_path,
            cargo_path=cargo_path,
        )
        code = run_test(spec, shared_lib_dir, dry_run=dry_run, force=force)
        if code != 0 and exit_code == 0:
            exit_code = code
    return exit_code


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--crate", required=True, help="Crate directory (must contain Cargo.toml)"
    )
    parser.add_argument(
        "--configs",
        required=True,
        help="Glob pattern for shared TOML config files (e.g. ./configs/*.toml)",
    )
    parser.add_argument(
        "--output-root",
        default="./var_tests",
        help="Root output directory (tests are grouped by config/crate)",
    )
    parser.add_argument("--profile", default="release")
    parser.add_argument("--leafc", default="leafc", help="Path to leafc executable")
    parser.add_argument(
        "--shared-lib-dir",
        help="Directory to prepend to LD_LIBRARY_PATH",
        default="/workspaces/Rust/leaf/leaf/target/release/runtime_noop",
    )
    parser.add_argument("--cargo", default="cargo", help="Path to cargo executable")
    parser.add_argument("--dry-run", action="store_true", help="Print commands only")
    parser.add_argument(
        "--force",
        action="store_true",
        help="Delete existing output for each config/crate and re-run tests",
    )
    args, target_args = parser.parse_known_args()
    args.target_args = target_args
    return args


def main() -> int:
    args = parse_args()

    crate = Path(args.crate).resolve()
    configs = list_config_files(args.configs)
    output_root = Path(args.output_root).resolve()
    shared_lib_dir = Path(args.shared_lib_dir).resolve()

    if not shared_lib_dir.is_dir():
        print(
            f"error: shared library directory does not exist: {shared_lib_dir}",
            file=sys.stderr,
        )
        return 2

    return run_test_matrix(
        crate=crate,
        configs=configs,
        output_root=output_root,
        target_args=args.target_args,
        profile=args.profile,
        package_name=crate.name,
        leafc_path=args.leafc,
        shared_lib_dir=args.shared_lib_dir,
        cargo_path=args.cargo,
        dry_run=args.dry_run,
        force=args.force,
    )


if __name__ == "__main__":
    raise SystemExit(main())
