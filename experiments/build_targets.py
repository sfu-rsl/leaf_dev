#!/usr/bin/env python3
"""Build all targets listed in a TOML file across shared config TOMLs."""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Mapping, Sequence

from build_matrix import list_config_files, run_build_matrix

try:
    import tomllib
except ModuleNotFoundError as exc:  # pragma: no cover
    raise SystemExit("Python 3.11+ is required (tomllib unavailable).") from exc


@dataclass(frozen=True)
class TargetSpec:
    crate: str
    target_args: list[str]
    profiles: list[str]
    enabled: bool = True
    crate_dir: str | None = None
    package_name: str | None = None


def _as_str_list(value: Any, field_name: str, crate: str) -> list[str]:
    if not isinstance(value, list) or not all(isinstance(x, str) for x in value):
        raise ValueError(
            f"target '{crate}' has invalid '{field_name}': expected list[str]"
        )
    return list(value)


def _as_profiles(value: Any, field_name: str, crate: str) -> list[str]:
    if isinstance(value, str):
        if not value.strip():
            raise ValueError(
                f"target '{crate}' has invalid '{field_name}': expected non-empty str"
            )
        return [value]

    if not isinstance(value, list) or not all(isinstance(x, str) for x in value):
        raise ValueError(
            f"target '{crate}' has invalid '{field_name}': expected str or list[str]"
        )

    cleaned = [x for x in value if x.strip()]
    if not cleaned:
        raise ValueError(
            f"target '{crate}' has invalid '{field_name}': expected non-empty list[str]"
        )
    return cleaned


def parse_targets_file(targets_path: Path) -> tuple[list[TargetSpec], dict[str, Any]]:
    with targets_path.open("rb") as f:
        data = tomllib.load(f)

    defaults_raw = data.get("defaults", {})
    if not isinstance(defaults_raw, Mapping):
        raise ValueError("'defaults' must be a table if provided")

    defaults: dict[str, Any] = dict(defaults_raw)
    if "profile" in defaults and "profiles" in defaults:
        raise ValueError("Use only one of defaults.profile or defaults.profiles")

    if "profiles" in defaults:
        default_profiles = _as_profiles(
            defaults["profiles"], "defaults.profiles", "defaults"
        )
    else:
        default_profiles = _as_profiles(
            defaults.get("profile", "release"),
            "defaults.profile",
            "defaults",
        )

    default_target_args = defaults.get("target_args", [])
    if "target_args" in defaults:
        default_target_args = _as_str_list(
            default_target_args,
            "defaults.target_args",
            "defaults",
        )
    else:
        default_target_args = []

    rows = data.get("target", [])
    if not isinstance(rows, list):
        raise ValueError("'target' must be an array of tables")

    parsed: list[TargetSpec] = []
    for i, row in enumerate(rows, start=1):
        if not isinstance(row, Mapping):
            raise ValueError(f"target[{i}] must be a table")

        crate = row.get("crate")
        if not isinstance(crate, str) or not crate.strip():
            raise ValueError(f"target[{i}] missing non-empty 'crate' field")

        enabled = row.get("enabled", True)
        if not isinstance(enabled, bool):
            raise ValueError(f"target[{i}] has invalid 'enabled': expected bool")

        if "profile" in row and "profiles" in row:
            raise ValueError(
                f"target[{i}] should set only one of 'profile' or 'profiles'"
            )

        if "profiles" in row:
            profiles = _as_profiles(row["profiles"], "profiles", crate)
        elif "profile" in row:
            profiles = _as_profiles(row["profile"], "profile", crate)
        else:
            profiles = list(default_profiles)

        if "target_args" in row:
            target_args = _as_str_list(row["target_args"], "target_args", crate)
        else:
            target_args = list(default_target_args)

        crate_dir = row.get("crate_dir")
        if crate_dir is not None and not isinstance(crate_dir, str):
            raise ValueError(f"target '{crate}' has invalid 'crate_dir': expected str")

        package_name = row.get("package_name")
        if package_name is not None and not isinstance(package_name, str):
            raise ValueError(
                f"target '{crate}' has invalid 'package_name': expected str"
            )

        parsed.append(
            TargetSpec(
                crate=crate,
                target_args=target_args,
                profiles=profiles,
                enabled=enabled,
                crate_dir=crate_dir,
                package_name=package_name,
            )
        )

    return parsed, defaults


def resolve_crate_dir(target: TargetSpec, crates_root: Path) -> Path:
    if target.crate_dir:
        return Path(target.crate_dir).expanduser().resolve()
    return (crates_root / target.crate).resolve()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--targets",
        default="./targets.toml",
        help="Path to targets TOML file",
    )
    parser.add_argument(
        "--configs",
        default="./configs/*.toml",
        help="Glob pattern for shared TOML config files",
    )
    parser.add_argument(
        "--crates-root",
        default="./crates",
        help="Root directory containing crate folders",
    )
    parser.add_argument(
        "--output-root",
        default="./var_builds",
        help="Output root directory (grouped by config/crate)",
    )
    parser.add_argument("--leafc", default="leafc", help="Path to leafc executable")
    parser.add_argument("--cargo", default="cargo", help="Path to cargo executable")
    parser.add_argument("--dry-run", action="store_true", help="Print commands only")
    parser.add_argument(
        "--force",
        action="store_true",
        help="Delete existing output and rebuild",
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Stop immediately on the first target failure",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()

    targets_path = Path(args.targets).resolve()
    crates_root = Path(args.crates_root).resolve()
    output_root = Path(args.output_root).resolve()
    configs = list_config_files(args.configs)

    targets, defaults = parse_targets_file(targets_path)

    only_enabled = defaults.get("only_enabled", True)
    if not isinstance(only_enabled, bool):
        raise ValueError("defaults.only_enabled must be a bool")

    exit_code = 0
    built_count = 0

    for target in targets:
        if only_enabled and not target.enabled:
            print(f"\n[skip] crate={target.crate} (disabled)")
            continue

        crate_dir = resolve_crate_dir(target, crates_root)
        print(
            f"\n[target] crate={target.crate} profiles={target.profiles} "
            f"args={target.target_args} package_name={target.package_name}"
        )

        code = 0
        for profile in target.profiles:
            profile_code = run_build_matrix(
                crate=crate_dir,
                configs=configs,
                output_root=output_root,
                target_args=target.target_args,
                profile=profile,
                package_name=target.package_name,
                leafc_path=args.leafc,
                cargo_path=args.cargo,
                dry_run=args.dry_run,
                force=args.force,
            )
            if profile_code != 0 and code == 0:
                code = profile_code
        built_count += 1

        if code != 0:
            exit_code = code
            print(f"[error] target '{target.crate}' failed with exit code {code}")
            if args.fail_fast:
                break

    if built_count == 0:
        print("No enabled targets found to build.")

    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
