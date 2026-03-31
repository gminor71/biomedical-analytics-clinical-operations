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
    Build a markdown batch summary for multiple CT07 binary method agent runs.
    """
    total_cases = len(results)
    success_count = sum(1 for r in results if r.get("status") == "success")
    unsupported_count = sum(1 for r in results if r.get("status") == "unsupported")

    lines: list[str] = [
        "# CT07 Binary Method Agent Batch Summary",
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
        status = result.get("status", "Unknown")

        decision = result.get("decision", {})
        endpoint_name = decision.get("endpoint_name", "Unknown")
        primary_method = decision.get("primary_method", "Not assigned")
        effect_measure = decision.get("effect_measure", "Not assigned")
        alternative_methods = decision.get("alternative_methods", [])
        warnings = decision.get("warnings", [])
        notes = decision.get("notes", [])

        lines.extend(
            [
                f"### {analysis_id}",
                "",
                f"- **Study ID:** {study_id}",
                f"- **Endpoint Type:** {endpoint_type}",
                f"- **Endpoint Name:** {endpoint_name}",
                f"- **Run Status:** {status}",
                f"- **Primary Method:** {primary_method}",
                f"- **Effect Measure:** {effect_measure}",
                "",
                "**Alternative Methods**",
                "",
                _format_list(alternative_methods),
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