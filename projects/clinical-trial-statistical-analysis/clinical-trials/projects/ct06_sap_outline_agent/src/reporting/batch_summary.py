from __future__ import annotations


def _format_list(items: list[str]) -> str:
    """
    Format a list of strings as markdown bullets.
    """
    if not items:
        return "- None"
    return "\n".join(f"- {item}" for item in items)


def build_batch_summary(results: list[dict]) -> str:
    """
    Build a markdown batch summary for multiple CT06 SAP outline runs.
    """
    total_cases = len(results)
    success_count = sum(1 for r in results if r.get("status") == "success")
    unsupported_count = sum(1 for r in results if r.get("status") == "unsupported")

    lines: list[str] = [
        "# CT06 SAP Outline Agent Batch Summary",
        "",
        "## Batch Overview",
        "",
        f"- **Total Cases:** {total_cases}",
        f"- **Successful Runs:** {success_count}",
        f"- **Unsupported Runs:** {unsupported_count}",
        "",
        "---",
        "",
        "## Case Summaries",
        "",
    ]

    for result in results:
        study_id = result.get("study_id", "Unknown")
        analysis_id = result.get("analysis_id", "Unknown")
        endpoint_type = result.get("endpoint_type", "Unknown")
        endpoint_name = result.get("endpoint_name", "Unknown")
        status = result.get("status", "Unknown")

        sap_outline = result.get("sap_outline", {})
        primary_method = sap_outline.get("primary_method", "Not assigned")
        sensitivity_analyses = sap_outline.get("sensitivity_analyses", [])
        follow_up_items = sap_outline.get("follow_up_items", [])

        warnings = result.get("warnings", [])
        notes = result.get("notes", [])

        lines.extend(
            [
                f"### {analysis_id}",
                "",
                f"- **Study ID:** {study_id}",
                f"- **Endpoint Type:** {endpoint_type}",
                f"- **Endpoint Name:** {endpoint_name}",
                f"- **Run Status:** {status}",
                f"- **Primary Method:** {primary_method}",
                "",
                "**Sensitivity Analyses**",
                "",
                _format_list(sensitivity_analyses),
                "",
                "**Follow-Up Items**",
                "",
                _format_list(follow_up_items),
                "",
                "**Warnings**",
                "",
                _format_list(warnings),
                "",
                "**Notes**",
                "",
                _format_list(notes),
                "",
                "---",
                "",
            ]
        )

    return "\n".join(lines)