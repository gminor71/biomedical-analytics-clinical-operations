from __future__ import annotations

import argparse
import yaml
from pathlib import Path

from src.controller import run_agent
from append_to_raid_log import append_triaged_item_to_raid_log


def load_yaml(file_path: Path) -> dict:
    with open(file_path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def save_yaml(data: dict, file_path: Path) -> None:
    file_path.parent.mkdir(parents=True, exist_ok=True)
    with open(file_path, "w", encoding="utf-8") as f:
        yaml.dump(data, f, sort_keys=False)


def resolve_input_path(input_value: str, base_dir: Path) -> Path:
    candidate = Path(input_value)

    if candidate.is_absolute():
        return candidate

    if candidate.exists():
        return candidate.resolve()

    return (base_dir / input_value).resolve()


def slugify(value: str) -> str:
    return (
        value.strip()
        .lower()
        .replace(" ", "_")
        .replace("/", "_")
        .replace("\\", "_")
        .replace("-", "_")
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run PM02 triage and append the result to a RAID log workbook."
    )
    parser.add_argument(
        "--input",
        required=True,
        help="PM02 input YAML file name or full path.",
    )
    parser.add_argument(
        "--raid-log",
        required=True,
        help="Path to raid_log_working.xlsx",
    )
    parser.add_argument(
        "--output",
        default="results",
        help="Folder name or full path for triaged YAML outputs.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    base_dir = Path(__file__).resolve().parent
    data_dir = base_dir / "data" / "processed"

    input_path = resolve_input_path(args.input, data_dir)
    raid_log_path = resolve_input_path(args.raid_log, base_dir)

    if not input_path.exists():
        raise FileNotFoundError(f"PM02 input file not found: {input_path}")

    if not raid_log_path.exists():
        raise FileNotFoundError(f"RAID log workbook not found: {raid_log_path}")

    output_candidate = Path(args.output)
    if output_candidate.is_absolute():
        output_dir = output_candidate
    else:
        output_dir = (base_dir / output_candidate).resolve()

    case_data = load_yaml(input_path)
    result = run_agent(case_data)

    item_slug = slugify(result.get("item_title", "triaged_item"))
    output_path = output_dir / f"{item_slug}_triaged.yaml"
    save_yaml(result, output_path)

    append_triaged_item_to_raid_log(output_path, raid_log_path)

    print(f"Saved triaged output: {output_path}")
    print(f"Updated RAID log: {raid_log_path}")


if __name__ == "__main__":
    main()