from __future__ import annotations

import json
from pathlib import Path
from typing import Any


def save_markdown_report(report_text: str, output_path: Path) -> None:
    """
    Save a markdown report to disk.

    Parameters
    ----------
    report_text : str
        Markdown report content.
    output_path : Path
        Destination file path.
    """
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with output_path.open("w", encoding="utf-8") as f:
        f.write(report_text)


def save_json_log(result: dict[str, Any], output_path: Path) -> None:
    """
    Save a structured JSON run log to disk.

    Parameters
    ----------
    result : dict[str, Any]
        Structured result dictionary.
    output_path : Path
        Destination file path.
    """
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with output_path.open("w", encoding="utf-8") as f:
        json.dump(result, f, indent=2, ensure_ascii=False)