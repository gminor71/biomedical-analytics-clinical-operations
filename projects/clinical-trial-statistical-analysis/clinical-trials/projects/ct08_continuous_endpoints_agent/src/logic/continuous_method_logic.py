from __future__ import annotations

from typing import Any


def evaluate_continuous_case(case: dict[str, Any]) -> dict[str, Any]:
    """
    Evaluate a continuous endpoint case and return method recommendations.
    """

    decision: dict[str, Any] = {
        "study_id": case.get("study_id"),
        "analysis_id": case.get("analysis_id"),
        "endpoint_type": case.get("endpoint_type"),
        "endpoint_name": case.get("endpoint_name"),
        "primary_method": None,
        "effect_measure": "Mean difference",
        "alternative_methods": [],
        "warnings": [],
        "notes": [],
    }

    endpoint_type = case.get("endpoint_type")
    n_groups = case.get("n_groups", 2)
    normality = case.get("normality", "unknown")
    variance = case.get("variance_homogeneity", "unknown")
    covariates = case.get("covariates", [])

    # --- Step 1: Confirm endpoint ---
    if endpoint_type != "continuous":
        decision["warnings"].append(
            f"Unsupported endpoint type: {endpoint_type}"
        )
        decision["notes"].append(
            "This agent supports only continuous endpoints."
        )
        return decision

    # --- Step 2: Baseline method ---
    if n_groups == 2:
        decision["primary_method"] = "Independent t-test"
    else:
        decision["primary_method"] = "ANOVA"

    # --- Step 3: Normality check ---
    if normality == "non_normal":
        decision["warnings"].append("Non-normal distribution detected.")
        decision["alternative_methods"].append(
            "Wilcoxon rank-sum test" if n_groups == 2 else "Kruskal-Wallis test"
        )
        decision["notes"].append(
            "Consider nonparametric methods due to distributional concerns."
        )

    # --- Step 4: Variance check ---
    if variance == "unequal" and n_groups == 2:
        decision["primary_method"] = "Welch t-test"
        decision["notes"].append(
            "Welch t-test preferred due to unequal variances."
        )

    # --- Step 5: Covariates ---
    if covariates:
        decision["alternative_methods"].append("Linear regression")
        decision["notes"].append(
            "Consider linear regression for covariate adjustment."
        )

    return decision