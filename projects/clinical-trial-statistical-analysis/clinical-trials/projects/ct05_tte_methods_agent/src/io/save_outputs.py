from __future__ import annotations

import json
from pathlib import Path
from typing import Any


def ensure_directory(path: str | Path) -> Path:
    """
    Ensure a directory exists and return it as a Path object.
    """
    directory = Path(path)
    directory.mkdir(parents=True, exist_ok=True)
    return directory


def save_markdown_report(report_text: str, output_path: str | Path) -> Path:
    """
    Save a markdown report to disk.

    Parameters
    ----------
    report_text : str
        Markdown content to save.
    output_path : str | Path
        Full file path for the markdown report.

    Returns
    -------
    Path
        Path to the saved report.
    """
    path = Path(output_path)
    ensure_directory(path.parent)

    with path.open("w", encoding="utf-8") as f:
        f.write(report_text)

    return path


def save_json_log(result: dict[str, Any], output_path: str | Path) -> Path:
    """
    Save the agent result as a JSON run log.

    Parameters
    ----------
    result : dict[str, Any]
        Structured agent result object.
    output_path : str | Path
        Full file path for the JSON log.

    Returns
    -------
    Path
        Path to the saved JSON log.
    """
    path = Path(output_path)
    ensure_directory(path.parent)

    with path.open("w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)

    return path