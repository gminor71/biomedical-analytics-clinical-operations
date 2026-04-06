from __future__ import annotations

from typing import Any


def _append_unique(target: list[str], items: list[str] | None) -> None:
    if not items:
        return
    for item in items:
        if item not in target:
            target.append(item)


def apply_rule_bundle(result: dict[str, list[str]], bundle: dict[str, list[str]] | None) -> None:
    if not bundle:
        return

    _append_unique(result["required_outputs"], bundle.get("required_outputs"))
    _append_unique(result["recommended_outputs"], bundle.get("recommended_outputs"))
    _append_unique(result["governance_recommendations"], bundle.get("governance_recommendations"))
    _append_unique(result["tracking_recommendations"], bundle.get("tracking_recommendations"))
    _append_unique(result["escalation_triggers"], bundle.get("escalation_triggers"))