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
        description="Process a batch of PM02 input YAML files generated from an MVR and append them to a RAID log."
    )
    parser.add_argument(
        "--input-dir",
        default="data/processed/generated_from_mvr",
        help="Folder containing PM02 input YAML files.",
    )
    parser.add_argument(
        "--raid-log",
        required=True,
        help="Path to raid_log_working.xlsx",
    )
    parser.add_argument(
        "--output-dir",
        default="results/generated_from_mvr",
        help="Folder for triaged YAML outputs.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    base_dir = Path(__file__).resolve().parent

    input_dir = resolve_input_path(args.input_dir, base_dir)
    raid_log_path = resolve_input_path(args.raid_log, base_dir)
    output_dir = resolve_input_path(args.output_dir, base_dir)

    if not input_dir.exists():
        raise FileNotFoundError(f"Input directory not found: {input_dir}")

    if not raid_log_path.exists():
        raise FileNotFoundError(f"RAID log workbook not found: {raid_log_path}")

    yaml_files = sorted(input_dir.glob("*.yaml"))

    if not yaml_files:
        raise ValueError(f"No YAML files found in: {input_dir}")

    processed_count = 0

    for input_file in yaml_files:
        case_data = load_yaml(input_file)
        result = run_agent(case_data)

        item_slug = slugify(result.get("item_title", input_file.stem))
        output_path = output_dir / f"{item_slug}_triaged.yaml"
        save_yaml(result, output_path)

        append_triaged_item_to_raid_log(output_path, raid_log_path)
        processed_count += 1

        print(f"Processed and appended: {input_file.name}")

    print(f"\nCompleted batch processing for {processed_count} items.")
    print(f"Triaged outputs saved in: {output_dir}")
    print(f"Updated RAID log: {raid_log_path}")


if __name__ == "__main__":
    main()