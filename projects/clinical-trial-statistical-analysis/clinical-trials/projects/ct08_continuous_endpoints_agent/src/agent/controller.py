from __future__ import annotations

from typing import Any

from framework.shared_logic.continuous_method_logic import evaluate_continuous_case


def run_agent(case: dict[str, Any]) -> dict[str, Any]:
    """
    Run the v1 continuous method agent on a single case.
    """
    endpoint_type = case.get("endpoint_type")

    result: dict[str, Any] = {
        "study_id": case.get("study_id"),
        "analysis_id": case.get("analysis_id"),
        "endpoint_type": endpoint_type,
        "status": "success",
        "decision": {},
    }

    if endpoint_type == "continuous":
        result["decision"] = evaluate_continuous_case(case)
    else:
        result["status"] = "unsupported"
        result["decision"] = {
            "warnings": [f"Unsupported endpoint type: {endpoint_type}"],
            "notes": ["This v1 agent currently supports only continuous endpoint logic."],
        }

    return result