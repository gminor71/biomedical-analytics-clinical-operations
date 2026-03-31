from __future__ import annotations

from typing import Any


def evaluate_sap_case(case: dict[str, Any]) -> dict[str, Any]:
    """
    Evaluate a structured SAP outline case and return a draft SAP analysis outline.

    Parameters
    ----------
    case : dict[str, Any]
        Structured input describing the analysis scenario.

    Returns
    -------
    dict[str, Any]
        Result object containing:
        - study metadata
        - SAP outline sections
        - warnings
        - notes
        - run status
    """
    result: dict[str, Any] = {
        "study_id": case.get("study_id"),
        "analysis_id": case.get("analysis_id"),
        "study_design": case.get("study_design"),
        "endpoint_type": case.get("endpoint_type"),
        "endpoint_name": case.get("endpoint_name"),
        "status": "success",
        "sap_outline": {
            "objective": "",
            "estimand": "",
            "analysis_population": "",
            "endpoint_definition": "",
            "primary_method": "",
            "covariate_strategy": "",
            "stratification": "",
            "missing_data_handling": "",
            "sensitivity_analyses": [],
            "follow_up_items": [],
        },
        "warnings": [],
        "notes": [],
    }

    study_design = case.get("study_design")
    endpoint_type = case.get("endpoint_type")
    endpoint_name = case.get("endpoint_name", "unspecified endpoint")
    objective = case.get("objective")
    estimand_strategy = case.get("estimand_strategy")
    analysis_population = case.get("analysis_population")
    stratification_factors = case.get("stratification_factors", [])
    covariates = case.get("covariates", [])
    intercurrent_events = case.get("intercurrent_events", [])
    missing_data_risk = case.get("missing_data_risk", "unknown")
    censoring_concern = case.get("censoring_concern", "unknown")
    ph_status = case.get("ph_assumption_status", "unknown")

    sap_outline = result["sap_outline"]

    # --- Step 1: Confirm supported endpoint type ---
    if endpoint_type != "time_to_event":
        result["status"] = "unsupported"
        result["warnings"].append(
            f"Unsupported endpoint type for SAP outline v1: {endpoint_type}"
        )
        result["notes"].append(
            "This v1 SAP outline agent currently supports only time-to-event endpoint scenarios."
        )
        return result

    # --- Step 2: Draft objective ---
    if objective:
        sap_outline["objective"] = objective
    else:
        sap_outline["objective"] = f"Compare treatment groups for {endpoint_name}."
        result["notes"].append(
            "Objective not explicitly provided; default wording applied."
        )

    # --- Step 3: Draft estimand ---
    if estimand_strategy:
        sap_outline["estimand"] = (
            f"Primary estimand strategy: {estimand_strategy}."
        )
    else:
        sap_outline["estimand"] = "Estimand strategy not fully specified."
        sap_outline["follow_up_items"].append(
            "Specify estimand strategy and handling of intercurrent events."
        )

    # --- Step 4: Draft analysis population ---
    if analysis_population:
        sap_outline["analysis_population"] = analysis_population.upper()
    else:
        sap_outline["analysis_population"] = "Not specified"
        result["warnings"].append("Analysis population not specified.")
        sap_outline["follow_up_items"].append(
            "Clarify primary analysis population (e.g., ITT, mITT, per-protocol)."
        )

    # --- Step 5: Draft endpoint definition ---
    sap_outline["endpoint_definition"] = (
        f"{endpoint_name} will be analyzed as a time-to-event endpoint."
    )

    # --- Step 6: Assign primary method ---
    sap_outline["primary_method"] = "Kaplan-Meier + Log-rank + Cox PH"

    # --- Step 7: Draft covariate strategy ---
    if covariates:
        sap_outline["covariate_strategy"] = (
            "Adjust for the following baseline covariates: "
            + ", ".join(covariates)
            + "."
        )
    else:
        sap_outline["covariate_strategy"] = "No baseline covariates specified."
        result["notes"].append(
            "No covariates were provided for the primary adjusted analysis."
        )

    # --- Step 8: Draft stratification ---
    if stratification_factors:
        sap_outline["stratification"] = (
            "Stratification factors: " + ", ".join(stratification_factors) + "."
        )
    else:
        sap_outline["stratification"] = "No stratification factors specified."
        result["notes"].append("No stratification factors were provided.")

    # --- Step 9: Draft missing data handling ---
    sap_outline["missing_data_handling"] = (
        f"Missing data risk is assessed as {missing_data_risk}; "
        f"censoring concern is {censoring_concern}."
    )

    # --- Step 10: Add sensitivity analyses based on scenario ---
    if ph_status in {"unknown", None}:
        sap_outline["sensitivity_analyses"].append(
            "Assess proportional hazards assumption and consider RMST if the assumption is not supported."
        )
        sap_outline["follow_up_items"].append(
            "Confirm proportional hazards diagnostics for the primary regression approach."
        )

    if ph_status == "violated":
        result["warnings"].append("Proportional hazards assumption violated.")
        sap_outline["sensitivity_analyses"].extend(
            [
                "Consider Restricted Mean Survival Time (RMST) analysis.",
                "Consider Accelerated Failure Time (AFT) modeling.",
                "Consider time-varying covariate or stratified Cox approaches.",
            ]
        )

    if censoring_concern != "non_informative":
        result["warnings"].append(
            "Potential informative censoring detected; sensitivity analysis should be considered."
        )
        sap_outline["sensitivity_analyses"].append(
            "Consider sensitivity analysis addressing informative censoring."
        )

    if missing_data_risk in {"moderate", "high"}:
        sap_outline["sensitivity_analyses"].append(
            "Review the impact of missing data assumptions on the primary analysis."
        )

    # --- Step 11: Intercurrent events follow-up ---
    if intercurrent_events:
        result["notes"].append(
            "Intercurrent events were provided and should be aligned with the estimand strategy."
        )
    else:
        sap_outline["follow_up_items"].append(
            "Clarify relevant intercurrent events and how they will be handled in the estimand."
        )

    # --- Step 12: Optional design note ---
    if study_design:
        result["notes"].append(f"Study design recorded as: {study_design}.")
    else:
        result["notes"].append("Study design not explicitly provided.")

    return result