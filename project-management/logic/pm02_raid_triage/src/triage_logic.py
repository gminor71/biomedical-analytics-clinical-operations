from __future__ import annotations

from typing import Any


def classify_item(case: dict[str, Any]) -> tuple[str, list[str]]:
    rationale: list[str] = []

    if case.get("decision_needed", False):
        rationale.append("decision_required_for_progress")
        return "Decision", rationale

    if case.get("issue_is_current", False):
        rationale.append("item_is_current_problem")
        return "Issue", rationale

    timeline_impact = case.get("timeline_impact", "low")
    deliverable_impact = case.get("deliverable_impact", "low")

    if timeline_impact in {"medium", "high"} or deliverable_impact in {"medium", "high"}:
        rationale.append("potential_future_delivery_threat")
        return "Risk", rationale

    rationale.append("task_or_follow_up_item")
    return "Action", rationale


def determine_priority(case: dict[str, Any]) -> tuple[str, list[str]]:
    rationale: list[str] = []

    timeline_impact = case.get("timeline_impact", "low")
    deliverable_impact = case.get("deliverable_impact", "low")
    regulatory_environment = case.get("regulatory_environment", "low")

    if timeline_impact == "high" and deliverable_impact == "high" and regulatory_environment == "high":
        rationale.extend([
            "high_timeline_impact",
            "high_deliverable_impact",
            "high_regulatory_environment",
        ])
        return "Critical", rationale

    if timeline_impact == "high" or deliverable_impact == "high":
        rationale.extend([
            "material_project_impact",
        ])
        return "High", rationale

    if timeline_impact == "medium" or deliverable_impact == "medium":
        rationale.append("moderate_project_impact")
        return "Medium", rationale

    rationale.append("limited_project_impact")
    return "Low", rationale


def determine_escalation(case: dict[str, Any], priority: str) -> tuple[bool, list[str]]:
    rationale: list[str] = []

    if priority == "Critical":
        rationale.append("critical_priority_requires_escalation")
        return True, rationale

    if priority == "High" and case.get("regulatory_environment") == "high":
        rationale.append("regulated_high_priority_item")
        return True, rationale

    if case.get("vendor_related", False) and case.get("timeline_impact") == "high":
        rationale.append("vendor_dependency_threatens_timeline")
        return True, rationale

    return False, rationale


def recommended_actions(classification: str, escalation_flag: bool) -> list[str]:
    actions = ["assign_named_owner"]

    if classification in {"Risk", "Issue", "Action"}:
        actions.append("define_resolution_due_date")

    if classification == "Decision":
        actions.append("identify_approver")

    if classification in {"Risk", "Issue"}:
        actions.append("review_in_next_status_meeting")

    if escalation_flag:
        actions.append("escalate_if_unresolved")

    return actions

def build_triage_note(
    classification: str,
    priority: str,
    case: dict[str, Any],
    escalation_flag: bool,
) -> str:
    parts: list[str] = []

    # Base classification
    parts.append(f"{classification}")

    # Impact summary
    timeline_impact = case.get("timeline_impact", "low")
    deliverable_impact = case.get("deliverable_impact", "low")

    if timeline_impact == "high" or deliverable_impact == "high":
        parts.append("high-impact")

    # Regulatory context
    if case.get("regulatory_environment") == "high":
        parts.append("regulated")

    # Combine base sentence
    note = " ".join(parts)

    # Add action-oriented ending
    if escalation_flag:
        note += "; escalation recommended"
    elif priority in {"High", "Critical"}:
        note += "; prompt action recommended"
    else:
        note += "; monitor and manage"

    return note