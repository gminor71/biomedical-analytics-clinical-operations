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

    test_files = [
        "test_case_a_execution.yaml",
        "test_case_b_planning.yaml",
        "test_case_c_override.yaml",
    ]

    for file_name in test_files:
        input_path = input_dir / file_name
        case_data = load_yaml(input_path)

        result = run_agent(case_data)

        output_path = output_dir / file_name.replace(".yaml", "_output.yaml")
        save_yaml(result, output_path)

        print(f"Processed: {file_name} → {output_path.name}")


if __name__ == "__main__":
    main()