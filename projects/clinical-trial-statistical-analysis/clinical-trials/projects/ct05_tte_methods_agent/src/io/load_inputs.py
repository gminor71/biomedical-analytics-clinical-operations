from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml


REQUIRED_FIELDS = {
    "study_id",
    "analysis_id",
    "endpoint_type",
    "endpoint_name",
    "sample_size",
    "event_rate",
    "censoring_present",
    "censoring_concern",
    "ph_assumption_status",
}


def load_case(file_path: str | Path) -> dict[str, Any]:
    """
    Load a YAML input case for the TTE method agent.

    Parameters
    ----------
    file_path : str | Path
        Path to the YAML case file.

    Returns
    -------
    dict[str, Any]
        Parsed case data.

    Raises
    ------
    FileNotFoundError
        If the input file does not exist.
    ValueError
        If the file is empty or required fields are missing.
    """
    path = Path(file_path)

    if not path.exists():
        raise FileNotFoundError(f"Input file not found: {path}")

    with path.open("r", encoding="utf-8") as f:
        data = yaml.safe_load(f)

    if not data:
        raise ValueError(f"Input file is empty or invalid: {path}")

    missing_fields = REQUIRED_FIELDS - set(data.keys())
    if missing_fields:
        missing_str = ", ".join(sorted(missing_fields))
        raise ValueError(f"Missing required field(s): {missing_str}")

    return data