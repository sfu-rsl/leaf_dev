#!/usr/bin/env python3
"""Build Rust crates with leafc across shared TOML configurations."""

from __future__ import annotations

import argparse
import glob
import os
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence
from string import Template


@dataclass(frozen=True)
class BuildSpec:
    crate_dir: Path
    config_path: Path
    out_dir: Path
    target_args: Sequence[str]
    profile: str = "release"
    package_name: str | None = None
    leafc_path: str = "leafc"
    cargo_path: str = "cargo"


def list_config_files(configs_glob: str) -> list[Path]:
    """Return sorted TOML configuration files matched by a glob pattern."""
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
    """Read a TOML file as raw text for LEAFC_CONFIG_STR."""

    text = config_path.read_text(encoding="utf-8")

    if config_path.name.endswith(".template.toml"):
        # Replace template variables
        substitutions = {
            "CRATE_NAME": package_name.replace("-", "_"),
        }
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


def build_command(spec: BuildSpec) -> list[str]:
    """Create the cargo build command for one crate/config pair."""
    manifest = spec.crate_dir / "Cargo.toml"
    if not manifest.is_file():
        raise FileNotFoundError(
            f"Missing Cargo.toml in crate directory: {spec.crate_dir}"
        )

    cmd = [
        spec.cargo_path,
        "rustc",
        "--manifest-path",
        str(manifest),
        "--target-dir",
        str(spec.out_dir),
        "--profile",
        spec.profile,
        "--timings",
        "--message-format",
        "json",
        "--config",
        f"profile.{spec.profile}.split-debuginfo = 'packed'",
        "--config",
        f"profile.{spec.profile}.debug = 'limited'",
    ]

    if spec.target_args:
        cmd.extend(spec.target_args)

    # Rustc flags
    cmd.extend(
        [
            "--",
            "-Z",
            f"self-profile={str(spec.out_dir)}",
            "-Z",
            "self-profile-events=default,llvm",
            # "--emit=llvm-ir,mir",
            # "-Zverify-llvm-ir",
        ]
    )

    return cmd


def build_env(config_toml_text: str, leafc_path: str) -> dict[str, str]:
    """Prepare environment for leafc-driven cargo builds."""
    env = dict(os.environ)
    env.pop("RUSTC", None)
    env.pop("RUSTFLAGS", None)
    env.pop("LEAFC_CONFIG_STR", None)
    env.pop("LEAFC_CONFIG_STR_FMT", None)
    env.pop("LEAFC_LOG", None)

    env["RUSTC"] = leafc_path
    env["LEAFC_CONFIG_STR"] = config_toml_text
    env["LEAFC_CONFIG_STR_FMT"] = "toml"
    env["CARGO_INCREMENTAL"] = "false"

    # env["LEAFC_LOG"] = "info,pass_objects=off"
    return env


def has_existing_result(spec: BuildSpec) -> bool:
    """Return True when build artifacts already exist for this spec/profile."""
    profile_out_dir = spec.out_dir / spec.profile
    return profile_out_dir.is_dir() and any(profile_out_dir.iterdir())


def run_build(spec: BuildSpec, dry_run: bool = False, force: bool = False) -> int:
    """Run one cargo build and return the process return code."""
    if has_existing_result(spec):
        if not force:
            print(
                f"\n[skip] crate={spec.crate_dir} config={spec.config_path.name} out={spec.out_dir}"
            )
            print("       existing artifacts detected; use --force to rebuild")
            return 0

        print(
            f"\n[force] removing existing output for crate={spec.crate_dir} config={spec.config_path.name}"
        )
        print(f"       rm -rf {spec.out_dir}")
        if not dry_run:
            shutil.rmtree(spec.out_dir)

    spec.out_dir.mkdir(parents=True, exist_ok=True)
    output_log = spec.out_dir / "build.log"
    package_name = spec.package_name or spec.crate_dir.name
    config_text = load_config_text(spec.config_path, package_name, spec.out_dir)
    env = build_env(config_text, spec.leafc_path)
    cmd = build_command(spec)

    print(
        f"\n[build] crate={spec.crate_dir} config={spec.config_path.name} out={spec.out_dir}"
    )
    print("       ", " ".join(cmd))
    print(f"       output -> {output_log}")

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

    return completed.returncode


def run_build_matrix(
    crate: Path,
    configs: Sequence[Path],
    output_root: Path,
    target_args: Sequence[str],
    profile: str,
    package_name: str | None,
    leafc_path: str,
    cargo_path: str,
    dry_run: bool,
    force: bool,
) -> int:
    """Build one crate with every config into separate folders."""
    exit_code = 0
    crate_name = crate.resolve().name
    for config in configs:
        out_dir = (
            output_root / config.stem.replace(".template", "") / crate_name / profile
        )
        spec = BuildSpec(
            crate_dir=crate,
            config_path=config,
            out_dir=out_dir,
            target_args=target_args,
            profile=profile,
            package_name=package_name or crate_name,
            leafc_path=leafc_path,
            cargo_path=cargo_path,
        )
        code = run_build(spec, dry_run=dry_run, force=force)
        if code != 0 and exit_code == 0:
            exit_code = code
    return exit_code


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--crate",
        required=True,
        help="Crate directory (must contain Cargo.toml)",
    )
    parser.add_argument(
        "--configs",
        required=True,
        help="Glob pattern for shared TOML config files (e.g. ./configs/*.toml)",
    )
    parser.add_argument(
        "--output-root",
        default="./var_builds",
        help="Root output directory (builds are grouped by config/crate)",
    )

    parser.add_argument("--profile", default="release")
    parser.add_argument("--leafc", default="leafc", help="Path to leafc executable")
    parser.add_argument("--cargo", default="cargo", help="Path to cargo executable")
    parser.add_argument("--dry-run", action="store_true", help="Print commands only")
    parser.add_argument(
        "--force",
        action="store_true",
        help="Delete existing output for each config/crate and rebuild",
    )
    args, target_args = parser.parse_known_args()
    args.target_args = target_args
    return args


def main() -> int:
    args = parse_args()

    crate = Path(args.crate).resolve()
    configs = list_config_files(args.configs)
    output_root = Path(args.output_root).resolve()

    return run_build_matrix(
        crate=crate,
        configs=configs,
        output_root=output_root,
        target_args=args.target_args,
        profile=args.profile,
        package_name=crate.name,
        leafc_path=args.leafc,
        cargo_path=args.cargo,
        dry_run=args.dry_run,
        force=args.force,
    )


if __name__ == "__main__":
    raise SystemExit(main())
