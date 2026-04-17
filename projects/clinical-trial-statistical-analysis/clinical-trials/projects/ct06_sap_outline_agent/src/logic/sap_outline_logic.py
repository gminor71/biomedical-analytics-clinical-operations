from __future__ import annotations

from typing import Any

from framework.shared_logic.tte_method_logic import evaluate_tte_case
from framework.shared_logic.binary_method_logic import evaluate_binary_case
from framework.shared_logic.continuous_method_logic import evaluate_continuous_case

def _get_method_decision(case: dict[str, Any]) -> dict[str, Any]:
    """
    Route the case to the appropriate endpoint-specific method logic.
    """
    endpoint_type = case.get("endpoint_type")

    if endpoint_type == "time_to_event":
        return evaluate_tte_case(case)
    if endpoint_type == "binary":
        return evaluate_binary_case(case)
    if endpoint_type == "continuous":
        return evaluate_continuous_case(case)

    return {
        "endpoint_type": endpoint_type,
        "primary_method": None,
        "effect_measure": None,
        "alternative_methods": [],
        "warnings": [f"Unsupported endpoint type for SAP outline v2: {endpoint_type}"],
        "notes": ["No endpoint-specific method logic is available for this endpoint type."],
    }


def _build_endpoint_definition(endpoint_name: str, endpoint_type: str) -> str:
    """
    Standardize endpoint description for SAP outline text.
    """
    if endpoint_type == "time_to_event":
        return f"{endpoint_name} will be analyzed as a time-to-event endpoint."
    if endpoint_type == "binary":
        return f"{endpoint_name} will be analyzed as a binary endpoint."
    if endpoint_type == "continuous":
        return f"{endpoint_name} will be analyzed as a continuous endpoint."
    return f"{endpoint_name} will be analyzed according to endpoint-specific methods."


def _add_method_driven_sensitivity_analyses(
    sap_outline: dict[str, Any],
    case: dict[str, Any],
    method_decision: dict[str, Any],
) -> None:
    """
    Add sensitivity analyses based on endpoint-specific method logic and case context.
    """
    endpoint_type = case.get("endpoint_type")
    ph_status = case.get("ph_assumption_status", "unknown")
    censoring_concern = case.get("censoring_concern", "unknown")
    missing_data_risk = case.get("missing_data_risk", "unknown")
    normality = case.get("normality", "unknown")
    variance = case.get("variance_homogeneity", "unknown")
    event_rate = case.get("event_rate", None)

    alternative_methods = method_decision.get("alternative_methods", [])

    if endpoint_type == "time_to_event":
        if ph_status in {"unknown", None}:
            sap_outline["sensitivity_analyses"].append(
                "Assess proportional hazards assumption and consider RMST if the assumption is not supported."
            )
            sap_outline["follow_up_items"].append(
                "Confirm proportional hazards diagnostics for the primary regression approach."
            )

        if ph_status == "violated":
            for method in alternative_methods:
                sap_outline["sensitivity_analyses"].append(
                    f"Consider {method} as an alternative time-to-event analysis approach."
                )

        if censoring_concern != "non_informative":
            sap_outline["sensitivity_analyses"].append(
                "Consider sensitivity analysis addressing informative censoring."
            )

    if endpoint_type == "binary":
        if alternative_methods:
            for method in alternative_methods:
                sap_outline["sensitivity_analyses"].append(
                    f"Consider {method} as an adjusted or alternative binary endpoint analysis."
                )

        if event_rate is not None and event_rate < 0.10:
            sap_outline["sensitivity_analyses"].append(
                "Consider exact or penalized methods if rare-event instability affects inference."
            )

    if endpoint_type == "continuous":
        if normality == "non_normal":
            for method in alternative_methods:
                if method != "Linear regression":
                    sap_outline["sensitivity_analyses"].append(
                        f"Consider {method} due to non-normality."
                    )

        if variance == "unequal":
            sap_outline["sensitivity_analyses"].append(
                "Confirm robustness of inference under unequal variance assumptions."
            )

        if "Linear regression" in alternative_methods:
            sap_outline["sensitivity_analyses"].append(
                "Consider adjusted linear regression including prespecified baseline covariates."
            )

    if missing_data_risk in {"moderate", "high"}:
        sap_outline["sensitivity_analyses"].append(
            "Review the impact of missing data assumptions on the primary analysis."
        )


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
            "effect_measure": "",
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

    sap_outline = result["sap_outline"]

    # --- Step 1: Confirm supported endpoint type via integrated method routing ---
    method_decision = _get_method_decision(case)

    if method_decision.get("primary_method") is None:
        result["status"] = "unsupported"
        result["warnings"].extend(method_decision.get("warnings", []))
        result["notes"].extend(method_decision.get("notes", []))
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
        sap_outline["estimand"] = f"Primary estimand strategy: {estimand_strategy}."
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
    sap_outline["endpoint_definition"] = _build_endpoint_definition(
        endpoint_name, endpoint_type
    )

    # --- Step 6: Pull in endpoint-specific primary method and effect measure ---
    sap_outline["primary_method"] = method_decision.get(
        "primary_method", "Method not specified"
    )

    sap_outline["effect_measure"] = method_decision.get("effect_measure") or ""

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
    if endpoint_type == "time_to_event":
        sap_outline["missing_data_handling"] = (
            f"Missing data risk is assessed as {missing_data_risk}; "
            f"censoring concern is {censoring_concern}."
        )
    else:
        sap_outline["missing_data_handling"] = (
            f"Missing data risk is assessed as {missing_data_risk}."
        )

    # --- Step 10: Add integrated method-driven sensitivity analyses ---
    _add_method_driven_sensitivity_analyses(sap_outline, case, method_decision)

    # --- Step 11: Intercurrent events follow-up ---
    if intercurrent_events:
        result["notes"].append(
            "Intercurrent events were provided and should be aligned with the estimand strategy."
        )
    else:
        sap_outline["follow_up_items"].append(
            "Clarify relevant intercurrent events and how they will be handled in the estimand."
        )

    # --- Step 12: Carry forward method-agent warnings and notes ---
    result["warnings"].extend(method_decision.get("warnings", []))
    result["notes"].extend(method_decision.get("notes", []))

    # --- Step 13: Optional design note ---
    if study_design:
        result["notes"].append(f"Study design recorded as: {study_design}.")
    else:
        result["notes"].append("Study design not explicitly provided.")

    return result