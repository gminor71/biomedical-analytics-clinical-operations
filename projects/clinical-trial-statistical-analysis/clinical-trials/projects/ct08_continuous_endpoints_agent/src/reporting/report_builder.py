from __future__ import annotations


def _format_list(items: list[str]) -> str:
    """
    Format a list of strings as markdown bullets.
    """
    if not items:
        return "- None"
    return "\n".join(f"- {item}" for item in items)


def build_markdown_report(result: dict) -> str:
    """
    Build a markdown report for a single CT08 continuous method agent run.
    """
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

    report = f"""# CT08 Continuous Endpoints Agent Report

## Run Summary

- **Study ID:** {study_id}
- **Analysis ID:** {analysis_id}
- **Endpoint Type:** {endpoint_type}
- **Endpoint Name:** {endpoint_name}
- **Run Status:** {status}

---

## Primary Recommendation

- **Recommended Method:** {primary_method}
- **Effect Measure:** {effect_measure}

---

## Alternative Methods

{_format_list(alternative_methods)}

---

## Warnings

{_format_list(warnings)}

---

## Notes

{_format_list(notes)}

---
"""

    return report