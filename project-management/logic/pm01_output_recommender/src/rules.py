BASE_REQUIRED_OUTPUTS = {
    "all": ["project_charter", "status_report"]
}

COMPLEXITY_RULES = {
    "medium": {
        "required_outputs": ["deliverables_tracker"],
        "recommended_outputs": ["milestone_tracker"],
    },
    "high": {
        "required_outputs": ["deliverables_tracker", "milestone_tracker"],
        "recommended_outputs": ["communication_plan"],
    },
}

REGULATORY_RULES = {
    "high": {
        "required_outputs": ["raid_log", "decision_log"],
    },
    "medium": {
        "recommended_outputs": ["raid_log"],
    },
}

VENDOR_RULES = {
    True: {
        "recommended_outputs": ["stakeholder_register", "communication_plan"],
        "escalation_triggers": ["vendor_dependency_threatens_timeline"],
    }
}

TIMELINE_RULES = {
    "high": {
        "governance_recommendations": ["weekly_core_team_status_meeting"],
        "escalation_triggers": ["milestone_delay_over_5_business_days"],
    },
    "medium": {
        "governance_recommendations": ["biweekly_core_team_status_meeting"],
    },
}

TEAM_STRUCTURE_RULES = {
    "cross_functional": {
        "recommended_outputs": ["communication_plan"],
        "governance_recommendations": ["monthly_stakeholder_update"],
    }
}

PHASE_RULES = {
    "planning": {
        "required_outputs": ["project_charter"],
        "recommended_outputs": [
            "stakeholder_register",
            "communication_plan",
            "milestone_tracker",
        ],
    },
    "execution": {
        "required_outputs": ["raid_log", "status_report", "deliverables_tracker"],
        "governance_recommendations": ["biweekly_risk_review"],
    },
    "closeout": {
        "recommended_outputs": ["lessons_learned_log", "closeout_checklist"],
    },
}

TEMPLATE_REFERENCE_MAP = {
    "project_charter": "../../../templates/project_charter_template.md",
    "raid_log": "../../../templates/raid_log_template.md",
    "decision_log": "../../../templates/decision_log_template.md",
    "status_report": "../../../templates/status_report_template.md",
    "deliverables_tracker": "../../../templates/deliverables_tracker_template.md",
    "communication_plan": "../../../templates/communication_plan_template.md",
    "stakeholder_register": "../../../templates/stakeholder_register_template.md",
    "milestone_tracker": "../../../templates/milestone_tracker_template.md",
}
