from __future__ import annotations

from typing import Any


def evaluate_tte_case(case: dict[str, Any]) -> dict[str, Any]:
    """
    Evaluate a time-to-event (TTE) case and return method recommendations.

    Parameters
    ----------
    case : dict[str, Any]
        Structured input describing the analysis scenario.

    Returns
    -------
    dict[str, Any]
        Decision object containing:
        - endpoint information
        - primary method recommendation
        - PH assumption status
        - alternative method suggestions
        - warnings
        - notes
    """
    decision: dict[str, Any] = {
        "study_id": case.get("study_id"),
        "analysis_id": case.get("analysis_id"),
        "endpoint_type": case.get("endpoint_type"),
        "endpoint_name": case.get("endpoint_name"),
        "primary_method": None,
        "ph_assumption_status": case.get("ph_assumption_status"),
        "alternative_methods": [],
        "warnings": [],
        "notes": [],
    }

    endpoint_type = case.get("endpoint_type")
    ph_status = case.get("ph_assumption_status")
    sample_size = case.get("sample_size", 0)
    event_rate = case.get("event_rate", 0.0)
    censoring_concern = case.get("censoring_concern", "unknown")

    # Step 1: Confirm endpoint type
    if endpoint_type != "time_to_event":
        decision["warnings"].append(
            f"Unsupported endpoint type for TTE logic: {endpoint_type}"
        )
        decision["notes"].append(
            "This v1 agent only supports time-to-event endpoint logic."
        )
        return decision

    # Step 2: Assign baseline recommendation
    decision["primary_method"] = "Kaplan-Meier + Log-rank + Cox PH"
    decision["notes"].append(
        "Default TTE framework selected: KM estimation, Log-rank comparison, and Cox PH regression."
    )

    # Step 3: Evaluate PH assumption
    if ph_status == "holds":
        decision["notes"].append(
            "Proportional hazards assumption appears satisfied."
        )
    elif ph_status == "violated":
        decision["warnings"].append("Proportional hazards assumption violated.")
        decision["alternative_methods"] = [
            "Stratified Cox model",
            "Time-varying covariates",
            "Restricted Mean Survival Time (RMST)",
            "Accelerated Failure Time (AFT) model",
        ]
        decision["notes"].append(
            "Alternative methods recommended due to PH assumption violation."
        )
    else:
        decision["warnings"].append("PH assumption status is unknown or not assessed.")
        decision["notes"].append(
            "Further diagnostics are needed before confirming the primary regression approach."
        )

    # Step 4: Evaluate sample size and event rate
    if sample_size < 50:
        decision["warnings"].append(
            "Small sample size may reduce model stability and interpretability."
        )

    if event_rate < 0.20:
        decision["warnings"].append(
            "Low event rate may reduce power and limit reliable inference."
        )

    # Step 5: Evaluate censoring concern
    if censoring_concern != "non_informative":
        decision["warnings"].append(
            "Potential informative censoring detected; sensitivity analysis should be considered."
        )

    return decision