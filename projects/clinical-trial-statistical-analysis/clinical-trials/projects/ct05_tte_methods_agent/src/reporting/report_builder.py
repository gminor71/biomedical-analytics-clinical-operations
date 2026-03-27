# CT05 TTE Method Agent Report

def _format_list(items: list[str]) -> str:
    """
    Format a list of strings as markdown bullets.
    """
    if not items:
        return "- None"
    return "\n".join(f"- {item}" for item in items)

def build_markdown_report(result: dict) -> str:

    # --- Extract values ---
    study_id = result.get("study_id", "Unknown")
    analysis_id = result.get("analysis_id", "Unknown")
    endpoint_type = result.get("endpoint_type", "Unknown")
    status = result.get("status", "Unknown")

    decision = result.get("decision", {})
    endpoint_name = decision.get("endpoint_name", "Unknown")
    primary_method = decision.get("primary_method", "Not assigned")
    ph_status = decision.get("ph_assumption_status", "Unknown")
    alternative_methods = decision.get("alternative_methods", [])
    warnings = decision.get("warnings", [])
    notes = decision.get("notes", [])

    # --- Build report ---
    report = f"""# CT05 TTE Method Agent Report

## Run Summary

- **Study ID:** {study_id}
- **Analysis ID:** {analysis_id}
- **Endpoint Type:** {endpoint_type}
- **Endpoint Name:** {endpoint_name}
- **Run Status:** {status}

---

## Primary Recommendation

- **Recommended Method:** {primary_method}
- **PH Assumption Status:** {ph_status}

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