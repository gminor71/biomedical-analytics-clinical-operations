from __future__ import annotations

import yaml
from pathlib import Path

from src.controller import run_agent


def load_yaml(file_path: Path) -> dict:
    with open(file_path, "r") as f:
        return yaml.safe_load(f)


def save_yaml(data: dict, file_path: Path) -> None:
    file_path.parent.mkdir(parents=True, exist_ok=True)
    with open(file_path, "w") as f:
        yaml.dump(data, f, sort_keys=False)


def main():
    base_dir = Path(__file__).resolve().parent
    input_dir = base_dir / "data" / "processed"
    output_dir = base_dir / "results"

    input_file = input_dir / "example_raid_item.yaml"
    output_file = output_dir / "triaged_example_item.yaml"

    case_data = load_yaml(input_file)
    result = run_agent(case_data)
    save_yaml(result, output_file)

    print(f"Processed: {input_file.name} → {output_file.name}")


if __name__ == "__main__":
    main()