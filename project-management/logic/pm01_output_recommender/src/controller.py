from __future__ import annotations

from typing import Any

from src.rules import (
    BASE_REQUIRED_OUTPUTS,
    COMPLEXITY_RULES,
    PHASE_RULES,
    REGULATORY_RULES,
    TEAM_STRUCTURE_RULES,
    TIMELINE_RULES,
    VENDOR_RULES,
    TEMPLATE_REFERENCE_MAP,
)
from src.recommendation_logic import apply_rule_bundle


def _build_template_references(
    required_outputs: list[str],
    recommended_outputs: list[str],
) -> dict[str, str]:
    template_references: dict[str, str] = {}

    for output_name in required_outputs + recommended_outputs:
        if output_name in TEMPLATE_REFERENCE_MAP and output_name not in template_references:
            template_references[output_name] = TEMPLATE_REFERENCE_MAP[output_name]

    return template_references


def run_agent(case: dict[str, Any]) -> dict[str, Any]:
    result = {
        "project_name": case.get("project_name", "Unnamed Project"),
        "required_outputs": [],
        "recommended_outputs": [],
        "governance_recommendations": [],
        "tracking_recommendations": [],
        "escalation_triggers": [],
        "template_references": {},
    }

    apply_rule_bundle(
        result,
        {"required_outputs": BASE_REQUIRED_OUTPUTS["all"]},
    )

    apply_rule_bundle(result, COMPLEXITY_RULES.get(case.get("complexity")))
    apply_rule_bundle(result, REGULATORY_RULES.get(case.get("regulatory_environment")))
    apply_rule_bundle(result, VENDOR_RULES.get(case.get("vendor_involvement")))
    apply_rule_bundle(result, TIMELINE_RULES.get(case.get("timeline_pressure")))
    apply_rule_bundle(result, TEAM_STRUCTURE_RULES.get(case.get("team_structure")))
    apply_rule_bundle(result, PHASE_RULES.get(case.get("phase")))

    primary_outputs = case.get("primary_outputs", [])

    if "risk_tracking" in primary_outputs and "raid_log" not in result["required_outputs"]:
        result["required_outputs"].append("raid_log")

    if "decision_tracking" in primary_outputs and "decision_log" not in result["required_outputs"]:
        result["required_outputs"].append("decision_log")

    if "deliverable_tracking" in primary_outputs and "deliverables_tracker" not in result["required_outputs"]:
        result["required_outputs"].append("deliverables_tracker")

    if "status_reporting" in primary_outputs and "status_report" not in result["required_outputs"]:
        result["required_outputs"].append("status_report")

    result["recommended_outputs"] = [
        item for item in result["recommended_outputs"]
        if item not in result["required_outputs"]
    ]

    result["template_references"] = _build_template_references(
        required_outputs=result["required_outputs"],
        recommended_outputs=result["recommended_outputs"],
    )

    return result