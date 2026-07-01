#!/usr/bin/env python3
"""Run runtime throughput benchmark for all targets from a targets TOML file."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import json
import os
import subprocess
import sys
from pathlib import Path
from typing import Any

from build_targets import TargetSpec, parse_targets_file


def _resolve_targets_path(explicit_path: str | None) -> Path:
    if explicit_path:
        return Path(explicit_path).resolve()

    candidates = [Path("./target.toml"), Path("./targets.toml")]
    for path in candidates:
        if path.is_file():
            return path.resolve()

    names = ", ".join(str(p) for p in candidates)
    raise FileNotFoundError(f"No targets file found. Tried: {names}")


def _resolve_profile(target: TargetSpec, profile_override: str | None) -> str:
    if profile_override:
        return profile_override
    return target.profile


def _select_targets(
    targets: list[TargetSpec],
    defaults: dict[str, Any],
    include_disabled: bool,
    crates: set[str],
) -> list[TargetSpec]:
    only_enabled = defaults.get("only_enabled", True)
    if not isinstance(only_enabled, bool):
        raise ValueError("defaults.only_enabled must be a bool")

    selected: list[TargetSpec] = []
    for target in targets:
        if crates and target.crate not in crates:
            continue

        if not include_disabled and only_enabled and not target.enabled:
            continue

        selected.append(target)

    return selected


def _try_parse_json_line(stdout_text: str) -> dict[str, Any] | None:
    line = stdout_text.strip()
    if line:
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            pass
        else:
            if isinstance(obj, dict):
                return obj

    for line in reversed(stdout_text.splitlines()):
        line = line.strip()
        if not line.startswith("{"):
            continue
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(obj, dict):
            return obj
    return None


def _build_run_result_cmd(
    args: argparse.Namespace,
    target: TargetSpec,
    config: str,
    profile: str,
) -> list[str]:
    cmd = [
        sys.executable,
        str(Path(__file__).with_name("run_result.py")),
        "--build-root",
        args.build_root,
        "--crate",
        target.crate,
        "--config",
        config,
        "--profile",
        profile,
        "--duration-seconds",
        str(args.duration_seconds),
        "--shared-lib-dir",
        args.shared_lib_dir,
    ]

    if args.program:
        cmd.extend(["--program", args.program])

    for arg in args.program_arg:
        cmd.extend(["--program-arg", arg])

    if args.show_program_output:
        cmd.append("--show-program-output")

    if args.continue_on_error:
        cmd.append("--continue-on-error")

    return cmd


def _resolve_configs(build_root: Path, requested_configs: list[str]) -> list[str]:
    if requested_configs:
        return requested_configs

    if not build_root.is_dir():
        raise FileNotFoundError(f"build root does not exist: {build_root}")

    configs = sorted(p.name for p in build_root.iterdir() if p.is_dir())
    if not configs:
        raise FileNotFoundError(
            f"no config directories found under build root: {build_root}"
        )
    return configs


def _append_jsonl_record(summary_file: Path, record: dict[str, Any]) -> None:
    line = json.dumps(record, sort_keys=True)
    with summary_file.open("a", encoding="utf-8") as fp:
        fp.write(line + "\n")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--targets",
        help="Targets TOML file; if omitted, auto-detects ./target.toml then ./targets.toml",
    )
    parser.add_argument(
        "--build-root",
        default="./var_builds",
        help="Root directory that contains config/crate/profile outputs",
    )
    parser.add_argument(
        "--config",
        action="append",
        default=[],
        help=(
            "Config name (e.g. full); can be repeated. "
            "If omitted, all configs under --build-root are used."
        ),
    )
    parser.add_argument(
        "--profile",
        help="Optional profile override for all targets (defaults to each target profile)",
    )
    parser.add_argument(
        "--crate",
        action="append",
        default=[],
        help="Only run selected crate(s); can be repeated",
    )
    parser.add_argument(
        "--include-disabled",
        action="store_true",
        help="Run disabled targets too",
    )
    parser.add_argument(
        "--duration-seconds",
        type=float,
        required=True,
        help="How long to keep executing each target program",
    )
    parser.add_argument(
        "--shared-lib-dir",
        default="/workspaces/Rust/leaf/leaf/target/release/runtime_noop",
        help="Directory to prepend to LD_LIBRARY_PATH",
    )
    parser.add_argument(
        "--program",
        help="Optional executable override applied to all targets",
    )
    parser.add_argument(
        "--program-arg",
        action="append",
        default=[],
        help="Argument passed to all executables; can be repeated",
    )
    parser.add_argument(
        "--show-program-output",
        action="store_true",
        help="Show benchmarked program stdout/stderr",
    )
    parser.add_argument(
        "--continue-on-error",
        action="store_true",
        help="Continue benchmark loops when a single process run fails",
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Stop on first target runner failure",
    )
    parser.add_argument(
        "--summary-file",
        help=(
            "Where to write the combined summary. "
            "Default: <build-root>/<config>/run_results_all.jsonl when one config is selected, "
            "otherwise <build-root>/run_results_all.jsonl. "
            "Output is append-only JSONL and checkpoints each result as one line."
        ),
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()

    targets_path = _resolve_targets_path(args.targets)
    targets, defaults = parse_targets_file(targets_path)

    crates = {x for x in args.crate if x}
    selected = _select_targets(
        targets=targets,
        defaults=defaults,
        include_disabled=args.include_disabled,
        crates=crates,
    )

    if not selected:
        print("No targets selected.", file=sys.stderr)
        return 2

    build_root = Path(args.build_root).resolve()
    try:
        configs = _resolve_configs(build_root, args.config)
    except (ValueError, FileNotFoundError) as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2

    default_name = "run_results_all.jsonl"
    summary_file = (
        Path(args.summary_file).resolve()
        if args.summary_file
        else (
            (build_root / configs[0] / default_name)
            if len(configs) == 1
            else (build_root / default_name)
        )
    )
    summary_file.parent.mkdir(parents=True, exist_ok=True)

    run_id = f"{datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ')}-{os.getpid()}"
    start_record: dict[str, Any] = {
        "type": "run_start",
        "run_id": run_id,
        "timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "targets_file": str(targets_path),
        "build_root": str(build_root),
        "configs": configs,
        "duration_seconds": args.duration_seconds,
        "summary_file": str(summary_file),
    }
    try:
        _append_jsonl_record(summary_file, start_record)
    except OSError as exc:
        print(f"error: failed to write jsonl start record: {exc}", file=sys.stderr)
        return 2

    exit_code = 0
    stop = False
    for config in configs:
        for target in selected:
            profile = _resolve_profile(target, args.profile)
            cmd = _build_run_result_cmd(
                args=args,
                target=target,
                config=config,
                profile=profile,
            )

            print(f"[run] config={config} crate={target.crate} profile={profile}")
            completed = subprocess.run(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )

            parsed_report = _try_parse_json_line(completed.stdout)
            row: dict[str, Any] = {
                "config": config,
                "crate": target.crate,
                "profile": profile,
                "command": cmd,
                "return_code": completed.returncode,
                "report": parsed_report,
            }
            if completed.stderr.strip():
                row["stderr"] = completed.stderr.strip()
            if completed.returncode != 0 and completed.stdout.strip():
                row["stdout"] = completed.stdout.strip()

            result_record: dict[str, Any] = {
                "type": "result",
                "run_id": run_id,
                "timestamp_utc": datetime.now(timezone.utc).isoformat(),
                **row,
            }
            try:
                _append_jsonl_record(summary_file, result_record)
            except OSError as exc:
                print(
                    f"error: failed to append jsonl result record: {exc}",
                    file=sys.stderr,
                )
                return 2

            if completed.returncode != 0 and exit_code == 0:
                exit_code = completed.returncode

            if completed.returncode != 0 and args.fail_fast:
                stop = True
                break

        if stop:
            break

    end_record: dict[str, Any] = {
        "type": "run_end",
        "run_id": run_id,
        "timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "return_code": exit_code,
    }
    try:
        _append_jsonl_record(summary_file, end_record)
    except OSError as exc:
        print(f"error: failed to append jsonl end record: {exc}", file=sys.stderr)
        return 2

    print(f"[summary] wrote {summary_file}")
    print(json.dumps({"summary_file": str(summary_file), "return_code": exit_code}))

    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
