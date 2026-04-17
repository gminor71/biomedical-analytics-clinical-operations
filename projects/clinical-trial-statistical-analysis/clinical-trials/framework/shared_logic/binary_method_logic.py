from __future__ import annotations

from typing import Any


def evaluate_binary_case(case: dict[str, Any]) -> dict[str, Any]:
    """
    Evaluate a binary endpoint case and return method recommendations.
    """

    decision: dict[str, Any] = {
        "study_id": case.get("study_id"),
        "analysis_id": case.get("analysis_id"),
        "endpoint_type": case.get("endpoint_type"),
        "endpoint_name": case.get("endpoint_name"),
        "primary_method": None,
        "effect_measure": None,
        "alternative_methods": [],
        "warnings": [],
        "notes": [],
    }

    endpoint_type = case.get("endpoint_type")
    sample_size = case.get("sample_size", 0)
    expected_cell_count = case.get("expected_cell_count", "adequate")
    covariates = case.get("covariates", [])
    event_rate = case.get("event_rate", None)

    # --- Step 1: Confirm endpoint ---
    if endpoint_type != "binary":
        decision["warnings"].append(
            f"Unsupported endpoint type: {endpoint_type}"
        )
        decision["notes"].append(
            "This agent supports only binary endpoints."
        )
        return decision

    # --- Step 2: Baseline method ---
    decision["primary_method"] = "Chi-square test"
    decision["effect_measure"] = "Risk difference / Risk ratio"

    # --- Step 3: Small sample / sparse data ---
    if sample_size < 50 or expected_cell_count == "low":
        decision["primary_method"] = "Fisher's exact test"
        decision["warnings"].append(
            "Small sample size or sparse data detected."
        )
        decision["notes"].append(
            "Fisher's exact test preferred due to low expected counts."
        )

    # --- Step 4: Covariate adjustment ---
    if covariates:
        decision["alternative_methods"].append("Logistic regression")
        decision["notes"].append(
            "Consider logistic regression for covariate adjustment."
        )
        decision["effect_measure"] = "Odds ratio"

    # --- Step 5: Rare event ---
    if event_rate is not None and event_rate < 0.1:
        decision["warnings"].append(
            "Rare event detected; effect estimates may be unstable."
        )
        decision["notes"].append(
            "Consider exact or penalized methods if instability is observed."
        )

    return decision