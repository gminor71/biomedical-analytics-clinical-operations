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
    Build a markdown report for a single SAP outline agent run.
    """
    # --- Extract top-level values ---
    study_id = result.get("study_id", "Unknown")
    analysis_id = result.get("analysis_id", "Unknown")
    endpoint_type = result.get("endpoint_type", "Unknown")
    endpoint_name = result.get("endpoint_name", "Unknown")
    status = result.get("status", "Unknown")

    sap_outline = result.get("sap_outline", {})
    objective = sap_outline.get("objective", "Not assigned")
    estimand = sap_outline.get("estimand", "Not assigned")
    analysis_population = sap_outline.get("analysis_population", "Not assigned")
    endpoint_definition = sap_outline.get("endpoint_definition", "Not assigned")
    primary_method = sap_outline.get("primary_method", "Not assigned")
    covariate_strategy = sap_outline.get("covariate_strategy", "Not assigned")
    stratification = sap_outline.get("stratification", "Not assigned")
    missing_data_handling = sap_outline.get("missing_data_handling", "Not assigned")
    sensitivity_analyses = sap_outline.get("sensitivity_analyses", [])
    follow_up_items = sap_outline.get("follow_up_items", [])

    warnings = result.get("warnings", [])
    notes = result.get("notes", [])

    # --- Build report ---
    report = f"""# CT06 SAP Outline Agent Report

## Run Summary

- **Study ID:** {study_id}
- **Analysis ID:** {analysis_id}
- **Endpoint Type:** {endpoint_type}
- **Endpoint Name:** {endpoint_name}
- **Run Status:** {status}

---

## Draft SAP Outline

### Objective

{objective}

### Estimand

{estimand}

### Analysis Population

{analysis_population}

### Endpoint Definition

{endpoint_definition}

### Primary Method

{primary_method}

### Covariate Strategy

{covariate_strategy}

### Stratification

{stratification}

### Missing Data Handling

{missing_data_handling}

---

## Sensitivity Analyses

{_format_list(sensitivity_analyses)}

---

## Follow-Up Items

{_format_list(follow_up_items)}

---

## Warnings

{_format_list(warnings)}

---

## Notes

{_format_list(notes)}

---
"""

    return report