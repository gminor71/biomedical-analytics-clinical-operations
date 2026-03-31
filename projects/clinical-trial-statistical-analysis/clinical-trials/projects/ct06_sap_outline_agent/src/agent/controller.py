from __future__ import annotations

from typing import Any

from src.logic.sap_outline_logic import evaluate_sap_case


def run_agent(case: dict[str, Any]) -> dict[str, Any]:
    """
    Run the v1 SAP outline agent on a single case.

    Parameters
    ----------
    case : dict[str, Any]
        Structured input case data.

    Returns
    -------
    dict[str, Any]
        Agent result containing:
        - input metadata
        - SAP outline output
        - run status
    """
    endpoint_type = case.get("endpoint_type")

    result: dict[str, Any] = {
        "study_id": case.get("study_id"),
        "analysis_id": case.get("analysis_id"),
        "endpoint_type": endpoint_type,
        "status": "success",
        "sap_outline": {},
        "warnings": [],
        "notes": [],
    }

    if endpoint_type == "time_to_event":
        logic_result = evaluate_sap_case(case)
        result.update(logic_result)
    else:
        result["status"] = "unsupported"
        result["warnings"] = [
            f"Unsupported endpoint type: {endpoint_type}"
        ]
        result["notes"] = [
            "This v1 SAP outline agent currently supports only time-to-event logic."
        ]

    return result