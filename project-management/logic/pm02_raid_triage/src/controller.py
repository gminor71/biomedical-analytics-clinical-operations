from __future__ import annotations

from typing import Any

from src.rules import OWNER_TYPE_MAP, URGENCY_MAP
from src.triage_logic import (
    classify_item,
    determine_priority,
    determine_escalation,
    recommended_actions,
    build_triage_note,
)


def run_agent(case: dict[str, Any]) -> dict[str, Any]:
    classification, class_rationale = classify_item(case)
    priority, priority_rationale = determine_priority(case)
    escalation_flag, escalation_rationale = determine_escalation(case, priority)

    owner_type = OWNER_TYPE_MAP.get(
        case.get("primary_function", "cross_functional"),
        "Cross-Functional Team",
    )

    due_date_urgency = URGENCY_MAP[priority]

    triage_note = build_triage_note(
        classification=classification,
        priority=priority,
        case=case,
        escalation_flag=escalation_flag,
    )

    rationale = class_rationale + priority_rationale + escalation_rationale

    return {
        "project_name": case.get("project_name", "Unnamed Project"),
        "item_title": case.get("item_title", "Unnamed Item"),
        "classification": classification,
        "priority": priority,
        "owner_type": owner_type,
        "escalation_flag": escalation_flag,
        "due_date_urgency": due_date_urgency,
        "suggested_status": "Open",
        "suggested_phase": case.get("project_phase", "Execution").capitalize(),
        "suggested_id_prefix": "RAID",
        "triage_note": triage_note,
        "rationale": rationale,
        "recommended_actions": recommended_actions(classification, escalation_flag),
    }
