#!/usr/bin/env python3
"""Convert aggregate benchmark JSON into append-friendly JSONL format."""

from __future__ import annotations

import argparse
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


def _derive_return_code(results: list[dict[str, Any]]) -> int:
    for row in results:
        code = row.get("return_code")
        if isinstance(code, int) and code != 0:
            return code
    return 0


def _load_aggregate(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError("input JSON must be an object")
    results = payload.get("results")
    if not isinstance(results, list):
        raise ValueError("input JSON must contain a 'results' list")
    for idx, row in enumerate(results):
        if not isinstance(row, dict):
            raise ValueError(f"results[{idx}] must be an object")
    return payload


def _default_output_path(input_path: Path) -> Path:
    if input_path.suffix.lower() == ".json":
        return input_path.with_suffix(".jsonl")
    return input_path.with_name(f"{input_path.name}.jsonl")


def _json_line(obj: dict[str, Any]) -> str:
    return json.dumps(obj, sort_keys=True)


def convert(input_path: Path, output_path: Path, run_id: str | None) -> int:
    aggregate = _load_aggregate(input_path)
    results = aggregate["results"]
    assert isinstance(results, list)

    resolved_run_id = (
        run_id or f"migrated-{datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ')}"
    )
    now = datetime.now(timezone.utc).isoformat()

    start_record: dict[str, Any] = {
        "type": "run_start",
        "run_id": resolved_run_id,
        "timestamp_utc": now,
        "targets_file": aggregate.get("targets_file"),
        "build_root": aggregate.get("build_root"),
        "configs": aggregate.get("configs", []),
        "duration_seconds": aggregate.get("duration_seconds"),
        "summary_file": str(output_path.resolve()),
        "source_summary_file": str(input_path.resolve()),
        "source_format": "aggregate_json",
    }

    lines: list[str] = [_json_line(start_record)]

    for row in results:
        assert isinstance(row, dict)
        result_record: dict[str, Any] = {
            "type": "result",
            "run_id": resolved_run_id,
            "timestamp_utc": now,
            **row,
        }
        lines.append(_json_line(result_record))

    end_record: dict[str, Any] = {
        "type": "run_end",
        "run_id": resolved_run_id,
        "timestamp_utc": now,
        "return_code": _derive_return_code(results),
    }
    lines.append(_json_line(end_record))

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return len(results)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", help="Path to the aggregate JSON summary")
    parser.add_argument(
        "--output",
        help="Path to output JSONL file (default: same name with .jsonl extension)",
    )
    parser.add_argument(
        "--run-id",
        help="Optional run id to stamp into all JSONL records",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    input_path = Path(args.input).resolve()
    output_path = (
        Path(args.output).resolve() if args.output else _default_output_path(input_path)
    )

    try:
        count = convert(
            input_path=input_path, output_path=output_path, run_id=args.run_id
        )
    except (OSError, ValueError, json.JSONDecodeError) as exc:
        print(f"error: {exc}")
        return 2

    print(
        json.dumps(
            {
                "input": str(input_path),
                "output": str(output_path),
                "results_converted": count,
            },
            sort_keys=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
