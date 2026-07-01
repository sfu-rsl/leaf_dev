#!/usr/bin/env python3
"""Run Miri for crates listed in a TOML targets file (or via --crate).

Usage examples:
  python run_miri.py                      # reads miri_targets.toml in current dir
  python run_miri.py --crate bufreader    # run only the bufreader crate
  python run_miri.py --show-output        # show program stdout/stderr

The script sets `MIRIFLAGS` by default to
"-Zmiri-disable-isolation -Zmiri-disable-stacked-borrows".

For each run, parses JSON output in format:
  {"benchmark": "name", "start_nanos": N, "end_nanos": N}

Persists results as JSONL with durations.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

try:  # Python 3.11+
    import tomllib  # type: ignore
except Exception:  # pragma: no cover - fallback to tomli if available
    import tomli as tomllib  # type: ignore


DEFAULT_MIRI_FLAGS = "-Zmiri-disable-isolation -Zmiri-disable-stacked-borrows"
DEFAULT_CRATES_ROOT = "perf/rustc-perf-miri/collector/runtime-benchmarks"


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument(
        "--targets-file",
        default="miri_targets.toml",
        help="TOML file listing targets (default: miri_targets.toml)",
    )
    p.add_argument(
        "--crates-root",
        default=DEFAULT_CRATES_ROOT,
        help=f"Root directory containing crate folders (default: {DEFAULT_CRATES_ROOT})",
    )
    p.add_argument(
        "--crate",
        action="append",
        default=[],
        help="Only run selected crate(s); can be repeated",
    )
    p.add_argument(
        "--miri-flags",
        default=DEFAULT_MIRI_FLAGS,
        help="Value to set for MIRIFLAGS environment variable",
    )
    p.add_argument(
        "--show-output",
        action="store_true",
        help="Show each Miri run stdout/stderr",
    )
    p.add_argument(
        "--fail-fast",
        action="store_true",
        help="Stop on first failure",
    )
    p.add_argument(
        "--summary-file",
        help=(
            "Where to append benchmark results in JSONL format. "
            "Default: <crates-root>/miri_results.jsonl"
        ),
    )
    return p.parse_args()


def load_targets(path: Path) -> list[tuple[str, bool]]:
    with path.open("rb") as fp:
        doc = tomllib.load(fp)

    targets: list[tuple[str, bool]] = []
    for entry in doc.get("target", []):
        crate = entry.get("crate")
        if not crate:
            continue
        enabled = entry.get("enabled", True)
        targets.append((crate, bool(enabled)))
    return targets


def _parse_json_line(line: str) -> dict | None:
    """Parse a JSON line, return dict or None if not valid JSON."""
    line = line.strip()
    if not line or not line.startswith("{"):
        return None
    try:
        return json.loads(line)
    except json.JSONDecodeError:
        return None


def _extract_benchmark_events(stdout_text: str) -> list[dict]:
    """Extract all benchmark event dicts from stdout."""
    events = []
    for line in stdout_text.splitlines():
        obj = _parse_json_line(line)
        if obj and "benchmark" in obj:
            events.append(obj)
    return events


def _compute_durations(events: list[dict]) -> list[dict]:
    """Compute durations from start_nanos/end_nanos pairs.

    Returns list of records like:
    {"benchmark": "name", "duration_nanos": N, "start_nanos": N, "end_nanos": N}
    """
    records = []
    pending_starts: dict[str, int] = {}
    for event in events:
        name = event.get("benchmark", "unknown")
        if "start_nanos" in event and "end_nanos" in event:
            start = event["start_nanos"]
            end = event["end_nanos"]
            duration = end - start
            records.append(
                {
                    "benchmark": name,
                    "duration_nanos": duration,
                    "start_nanos": start,
                    "end_nanos": end,
                }
            )
        elif "start_nanos" in event:
            pending_starts[name] = event["start_nanos"]
        elif "end_nanos" in event and name in pending_starts:
            start = pending_starts.pop(name)
            end = event["end_nanos"]
            duration = end - start
            records.append(
                {
                    "benchmark": name,
                    "duration_nanos": duration,
                    "start_nanos": start,
                    "end_nanos": end,
                }
            )
    return records


def _write_result_record(summary_file: Path, crate: str, record: dict) -> None:
    """Append a result record to the JSONL summary file."""
    with summary_file.open("a", encoding="utf-8") as fp:
        result = {
            "crate": crate,
            "timestamp": datetime.now(timezone.utc).isoformat(),
            **record,
        }
        fp.write(json.dumps(result) + "\n")


def run_miri_for(
    crate: str, crate_dir: Path, miri_flags: str, show_output: bool
) -> tuple[int, list[dict]]:
    """Run cargo miri run in the crate directory.

    Returns (exit_code, list_of_duration_records).
    """
    env = os.environ.copy()
    env["MIRIFLAGS"] = miri_flags

    cmd = ["cargo", "miri", "run"]
    print(f"==> Running crate: {crate}")
    print(f"Directory: {crate_dir}")
    print(f"Command: {' '.join(cmd)}")
    print(f"MIRIFLAGS={miri_flags}\n")

    if show_output:
        rc = subprocess.run(cmd, env=env, cwd=crate_dir).returncode
        return rc, []

    proc = subprocess.run(cmd, env=env, cwd=crate_dir, capture_output=True, text=True)
    if proc.stdout:
        print(proc.stdout)
    if proc.stderr:
        print(proc.stderr, file=sys.stderr)

    # Parse JSON output and compute durations
    events = _extract_benchmark_events(proc.stdout)
    durations = _compute_durations(events)

    return proc.returncode, durations


def main() -> int:
    args = parse_args()
    targets_file = Path(args.targets_file)
    if not targets_file.is_file():
        print(f"Targets file not found: {targets_file}")
        return 2

    crates_root = Path(args.crates_root).resolve()
    if not crates_root.is_dir():
        print(f"Crates root not found: {crates_root}")
        return 2

    # Determine summary file location
    if args.summary_file:
        summary_file = Path(args.summary_file).resolve()
    else:
        summary_file = Path("miri_results.jsonl").resolve()
    print(f"Using summary file: {summary_file}")

    # Create parent dir if needed
    summary_file.parent.mkdir(parents=True, exist_ok=True)

    targets = load_targets(targets_file)
    crates = [c for c, enabled in targets if enabled]
    if args.crate:
        crates = [c for c in crates if c in set(args.crate)]

    if not crates:
        print("No crates selected to run. Exiting.")
        return 0

    for crate in crates:
        crate_dir = crates_root / crate
        if not crate_dir.is_dir():
            print(f"Crate directory not found: {crate_dir}")
            if args.fail_fast:
                return 2
            continue

        rc, durations = run_miri_for(
            crate, crate_dir, args.miri_flags, args.show_output
        )

        # Write results
        for record in durations:
            _write_result_record(summary_file, crate, record)

        if rc != 0:
            print(f"crate={crate} failed with exit code {rc}")
            if args.fail_fast:
                return rc

    print(f"\nResults written to: {summary_file}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
