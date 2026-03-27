from __future__ import annotations

from typing import Any


def build_batch_summary(results: list[dict[str, Any]]) -> str:
    """
    Build a markdown summary report for a batch of agent runs.

    Parameters
    ----------
    results : list[dict[str, Any]]
        List of agent result objects.

    Returns
    -------
    str
        Markdown-formatted batch summary.
    """
    if not results:
        return "# CT05 Batch Summary\n\nNo results available.\n"

    lines: list[str] = [
        "# CT05 TTE Method Agent – Batch Summary",
        "",
        "## Overview",
        "",
        f"- Total cases processed: {len(results)}",
        "",
        "---",
        "",
        "## Case Summary",
        "",
        "| Analysis ID | Endpoint Name | Run Status | PH Status | Primary Method | Warnings |",
        "|---|---|---|---|---|---:|",
    ]

    for result in results:
        decision = result.get("decision", {})

        analysis_id = result.get("analysis_id", "Unknown")
        endpoint_name = decision.get("endpoint_name", "Unknown")
        status = result.get("status", "Unknown")
        ph_status = decision.get("ph_assumption_status", "Unknown")
        primary_method = decision.get("primary_method", "Not assigned")
        warning_count = len(decision.get("warnings", []))

        lines.append(
            f"| {analysis_id} | {endpoint_name} | {status} | {ph_status} | {primary_method} | {warning_count} |"
        )

    lines.extend([
        "",
        "---",
        "",
        "## Warning Details",
        "",
    ])

    for result in results:
        decision = result.get("decision", {})
        analysis_id = result.get("analysis_id", "Unknown")
        warnings = decision.get("warnings", [])

        lines.append(f"### {analysis_id}")
        if warnings:
            for warning in warnings:
                lines.append(f"- {warning}")
        else:
            lines.append("- No warnings")
        lines.append("")

    return "\n".join(lines)