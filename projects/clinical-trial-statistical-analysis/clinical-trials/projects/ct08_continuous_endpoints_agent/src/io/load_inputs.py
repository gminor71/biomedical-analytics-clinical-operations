from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml


def load_case(input_path: Path) -> dict[str, Any]:
    """
    Load a structured YAML case file.
    """
    if not input_path.exists():
        raise FileNotFoundError(f"Input file not found: {input_path}")

    with input_path.open("r", encoding="utf-8") as f:
        case = yaml.safe_load(f)

    if not isinstance(case, dict):
        raise ValueError(f"Expected YAML content to be a dictionary: {input_path}")

    return case