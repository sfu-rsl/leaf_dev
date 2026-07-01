#!/usr/bin/env python3
"""Run built runtime benchmark binaries once and extract JSON benchmark output."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import json
import os
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

from run_result import (
    build_env_with_ld_library_path,
    is_executable_file,
    resolve_program_path,
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--build-root",
        default="./perf_builds",
        help="Root directory that contains config/crate/profile outputs",
    )
    parser.add_argument(
        "--config",
        action="append",
        default=[],
        help="Only run selected config(s); can be repeated",
    )
    parser.add_argument(
        "--crate",
        action="append",
        default=[],
        help="Only run selected crate(s); can be repeated",
    )
    parser.add_argument(
        "--profile",
        action="append",
        default=[],
        help="Cargo profile(s) to run; can be repeated (default: release)",
    )
    parser.add_argument(
        "--exclude-profile",
        action="append",
        default=[],
        help="Exclude profile(s); can be repeated",
    )
    parser.add_argument(
        "--program",
        help=(
            "Optional executable override. If omitted, executable is resolved from build.log. "
            "If relative, it is resolved from the target output directory."
        ),
    )
    parser.add_argument(
        "--program-arg",
        action="append",
        default=[],
        help="Argument passed to each executable; can be repeated",
    )
    parser.add_argument(
        "--shared-lib-dir",
        help="Directory to prepend to LD_LIBRARY_PATH",
        default="/workspaces/Rust/leaf/leaf/target/release/runtime_noop",
    )
    parser.add_argument(
        "--show-program-output",
        action="store_true",
        help="Show each benchmark program stdout/stderr",
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Stop on first target failure",
    )
    parser.add_argument(
        "--summary-file",
        help=(
            "Where to append combined benchmark records in JSONL format. "
            "Default: <build-root>/<config>/benchmark_results_all.jsonl when one config is "
            "selected, otherwise <build-root>/benchmark_results_all.jsonl"
        ),
    )
    return parser.parse_args()


def _iter_targets(
    build_root: Path,
    allowed_profiles: set[str],
    excluded_profiles: set[str],
    allowed_configs: set[str],
    allowed_crates: set[str],
) -> list[tuple[str, str, Path]]:
    if not build_root.is_dir():
        raise FileNotFoundError(f"build root does not exist: {build_root}")

    targets: list[tuple[str, str, Path]] = []
    for config_dir in sorted(p for p in build_root.iterdir() if p.is_dir()):
        config = config_dir.name
        if allowed_configs and config not in allowed_configs:
            continue

        for crate_dir in sorted(p for p in config_dir.iterdir() if p.is_dir()):
            crate = crate_dir.name
            if allowed_crates and crate not in allowed_crates:
                continue

            # Iterate through all profile directories
            for profile_dir in sorted(p for p in crate_dir.iterdir() if p.is_dir()):
                profile = profile_dir.name
                if allowed_profiles and profile not in allowed_profiles:
                    continue
                if profile in excluded_profiles:
                    continue

                target_root = profile_dir / (profile if profile != "dev" else "debug")
                build_log = profile_dir / "build.log"
                if target_root.is_dir() and build_log.is_file():
                    targets.append((config, crate, target_root))

    return targets


def _try_parse_json_line(line: str) -> dict[str, Any] | None:
    line = line.strip()
    if not line.startswith("{"):
        return None

    try:
        obj = json.loads(line)
    except json.JSONDecodeError:
        return None

    if isinstance(obj, dict):
        return obj
    return None


def _extract_json_messages(stdout_text: str) -> list[dict[str, Any]]:
    messages: list[dict[str, Any]] = []
    for line in stdout_text.splitlines():
        obj = _try_parse_json_line(line)
        if obj is not None:
            messages.append(obj)
    return messages


def _extract_benchmark_results(messages: list[dict[str, Any]]) -> list[dict[str, Any]]:
    results: list[dict[str, Any]] = []
    for obj in messages:
        payload = obj.get("Result")
        if isinstance(payload, dict):
            results.append(payload)
    return results


def _write_per_target_report(
    report_dir: Path,
    report: dict[str, Any],
    file_name: str = "benchmark_result.jsonl",
) -> Path:
    path = report_dir / file_name
    with path.open("a", encoding="utf-8") as fp:
        fp.write(json.dumps(report, sort_keys=True) + "\n")
    return path


def _build_summary_file(
    args: argparse.Namespace,
    build_root: Path,
    selected_configs: list[str],
) -> Path:
    if args.summary_file:
        return Path(args.summary_file).resolve()

    if len(selected_configs) == 1:
        return (
            build_root / selected_configs[0] / "benchmark_results_all.jsonl"
        ).resolve()
    return (build_root / "benchmark_results_all.jsonl").resolve()


def _append_jsonl_record(summary_file: Path, record: dict[str, Any]) -> None:
    with summary_file.open("a", encoding="utf-8") as fp:
        fp.write(json.dumps(record, sort_keys=True) + "\n")


def _try_parse_timestamp_utc(value: Any) -> datetime | None:
    if not isinstance(value, str) or not value:
        return None

    try:
        parsed = datetime.fromisoformat(value)
    except ValueError:
        return None

    if parsed.tzinfo is None:
        return parsed.replace(tzinfo=timezone.utc)
    return parsed


def _load_latest_result_timestamps(
    summary_file: Path,
) -> dict[tuple[str, str, str], datetime]:
    latest: dict[tuple[str, str, str], datetime] = {}
    if not summary_file.is_file():
        return latest

    with summary_file.open("r", encoding="utf-8") as fp:
        for raw_line in fp:
            line = raw_line.strip()
            if not line:
                continue

            try:
                record = json.loads(line)
            except json.JSONDecodeError:
                continue

            if not isinstance(record, dict):
                continue

            if record.get("type") != "result":
                continue

            config = record.get("config")
            crate = record.get("crate")
            profile = record.get("profile")
            if not isinstance(config, str):
                continue
            if not isinstance(crate, str):
                continue
            if not isinstance(profile, str):
                continue

            timestamp = _try_parse_timestamp_utc(record.get("timestamp_utc"))
            if timestamp is None:
                continue

            key = (config, crate, profile)
            prev = latest.get(key)
            if prev is None or timestamp > prev:
                latest[key] = timestamp

    return latest


def _prioritize_targets(
    targets: list[tuple[str, str, Path]],
    latest_timestamps: dict[tuple[str, str, str], datetime],
) -> list[tuple[str, str, Path]]:
    def sort_key(target: tuple[str, str, Path]) -> tuple[int, datetime, str, str]:
        config, crate, target_root = target
        profile = target_root.name  # Extract profile from target_root
        timestamp = latest_timestamps.get((config, crate, profile))
        if timestamp is None:
            # Never-run targets come first.
            return (0, datetime.min.replace(tzinfo=timezone.utc), config, crate)
        return (1, timestamp, config, crate)

    return sorted(targets, key=sort_key)


def main() -> int:
    args = parse_args()

    build_root = Path(args.build_root).resolve()
    shared_lib_dir = Path(args.shared_lib_dir).resolve()

    if not shared_lib_dir.is_dir():
        print(
            f"error: shared library directory does not exist: {shared_lib_dir}",
            file=sys.stderr,
        )
        return 2

    allowed_configs = {x for x in args.config if x}
    allowed_crates = {x for x in args.crate if x}
    allowed_profiles = {x for x in args.profile if x}
    excluded_profiles = {x for x in args.exclude_profile if x}

    # Default to "release" profile if no profiles specified
    if not allowed_profiles:
        allowed_profiles = {"dev", "release"}

    try:
        targets = _iter_targets(
            build_root=build_root,
            allowed_profiles=allowed_profiles,
            excluded_profiles=excluded_profiles,
            allowed_configs=allowed_configs,
            allowed_crates=allowed_crates,
        )
    except FileNotFoundError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2

    if not targets:
        print("error: no benchmark targets found", file=sys.stderr)
        return 2

    selected_configs = sorted({config for config, _, _ in targets})
    summary_file = _build_summary_file(args, build_root, selected_configs)
    summary_file.parent.mkdir(parents=True, exist_ok=True)
    latest_timestamps = _load_latest_result_timestamps(summary_file)
    targets = _prioritize_targets(targets, latest_timestamps)

    env = build_env_with_ld_library_path(shared_lib_dir)
    run_id = f"{datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ')}-{os.getpid()}"
    selected_profiles = sorted({target_root.name for _, _, target_root in targets})
    start_record: dict[str, Any] = {
        "type": "run_start",
        "run_id": run_id,
        "timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "build_root": str(build_root),
        "configs": selected_configs,
        "profiles": selected_profiles,
        "program_args": args.program_arg,
        "shared_lib_dir": str(shared_lib_dir),
        "ld_library_path": env.get("LD_LIBRARY_PATH", ""),
        "summary_file": str(summary_file),
        "prioritized_by_previous_results": True,
    }
    try:
        _append_jsonl_record(summary_file, start_record)
    except OSError as exc:
        print(f"error: failed to append jsonl start record: {exc}", file=sys.stderr)
        return 2

    exit_code = 0
    stop = False
    for config, crate, target_root in targets:
        profile = target_root.name  # Extract profile from target_root
        build_log = target_root.parent / "build.log"

        try:
            program_path, resolved_target = resolve_program_path(
                target_root=target_root,
                build_log=build_log,
                program=args.program,
            )
        except (ValueError, FileNotFoundError) as exc:
            row = {
                "config": config,
                "crate": crate,
                "profile": profile,
                "return_code": 2,
                "error": str(exc),
                "build_log": str(build_log),
            }
            result_record: dict[str, Any] = {
                "type": "result",
                "run_id": run_id,
                "timestamp_utc": datetime.now(timezone.utc).isoformat(),
                **row,
            }
            try:
                _append_jsonl_record(summary_file, result_record)
            except OSError as append_exc:
                print(
                    f"error: failed to append jsonl result record: {append_exc}",
                    file=sys.stderr,
                )
                return 2
            if exit_code == 0:
                exit_code = 2
            if args.fail_fast:
                break
            continue

        if not is_executable_file(program_path):
            row = {
                "config": config,
                "crate": crate,
                "profile": profile,
                "return_code": 2,
                "error": f"program is not executable: {program_path}",
                "build_log": str(build_log),
                "program": str(program_path),
            }
            result_record = {
                "type": "result",
                "run_id": run_id,
                "timestamp_utc": datetime.now(timezone.utc).isoformat(),
                **row,
            }
            try:
                _append_jsonl_record(summary_file, result_record)
            except OSError as append_exc:
                print(
                    f"error: failed to append jsonl result record: {append_exc}",
                    file=sys.stderr,
                )
                return 2
            if exit_code == 0:
                exit_code = 2
            if args.fail_fast:
                break
            continue

        cmd = [str(program_path), *args.program_arg]
        print(f"[run] config={config} crate={crate} profile={profile}")
        start = time.perf_counter()
        completed = subprocess.run(
            cmd,
            cwd=program_path.parent,
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        elapsed = time.perf_counter() - start

        stdout_text = completed.stdout or ""
        stderr_text = completed.stderr or ""

        if args.show_program_output and stdout_text:
            print(stdout_text, end="")
        if args.show_program_output and stderr_text:
            print(stderr_text, end="", file=sys.stderr)

        messages = _extract_json_messages(stdout_text)
        benchmark_results = _extract_benchmark_results(messages)

        resolved_kind: str | None = None
        resolved_name: str | None = None
        if isinstance(resolved_target.get("kind"), list):
            kind_list = [x for x in resolved_target["kind"] if isinstance(x, str)]
            resolved_kind = kind_list[0] if kind_list else None
        if isinstance(resolved_target.get("name"), str):
            resolved_name = resolved_target["name"]

        report: dict[str, Any] = {
            "config": config,
            "crate": crate,
            "profile": profile,
            "target_kind": resolved_kind,
            "target_name": resolved_name,
            "program": str(program_path),
            "program_args": args.program_arg,
            "return_code": completed.returncode,
            "elapsed_seconds": elapsed,
            "build_log": str(build_log),
            "json_messages": messages,
            "benchmark_results": benchmark_results,
            "benchmark_count": len(benchmark_results),
        }
        if stderr_text.strip():
            report["stderr"] = stderr_text.strip()

        per_target_path = _write_per_target_report(target_root.parent, report)
        report["report_file"] = str(per_target_path)
        result_record = {
            "type": "result",
            "run_id": run_id,
            "timestamp_utc": datetime.now(timezone.utc).isoformat(),
            **report,
        }
        try:
            _append_jsonl_record(summary_file, result_record)
        except OSError as append_exc:
            print(
                f"error: failed to append jsonl result record: {append_exc}",
                file=sys.stderr,
            )
            return 2

        if completed.returncode != 0 and exit_code == 0:
            exit_code = completed.returncode
        if completed.returncode != 0 and args.fail_fast:
            stop = True
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
