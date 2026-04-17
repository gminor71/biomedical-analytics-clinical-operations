from __future__ import annotations

from typing import Any

from framework.shared_logic.tte_method_logic import evaluate_tte_case


def run_agent(case: dict[str, Any]) -> dict[str, Any]:
    """
    Run the v1 biostat method agent on a single case.

    Parameters
    ----------
    case : dict[str, Any]
        Structured input case data.

    Returns
    -------
    dict[str, Any]
        Agent result containing:
        - input metadata
        - decision output
        - run status
    """
    endpoint_type = case.get("endpoint_type")

    result: dict[str, Any] = {
        "study_id": case.get("study_id"),
        "analysis_id": case.get("analysis_id"),
        "endpoint_type": endpoint_type,
        "status": "success",
        "decision": {},
    }

    if endpoint_type == "time_to_event":
        result["decision"] = evaluate_tte_case(case)
    else:
        result["status"] = "unsupported"
        result["decision"] = {
            "warnings": [f"Unsupported endpoint type: {endpoint_type}"],
            "notes": ["This v1 agent currently supports only time-to-event logic."],
        }

    return result