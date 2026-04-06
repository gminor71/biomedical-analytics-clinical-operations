# PM02 RAID Triage

This module provides lightweight triage logic for project items that may need
to be tracked in a RAID log.

Given a structured item profile, it recommends:
- classification
- priority
- owner type
- escalation flag
- due date urgency
- recommended next actions

## Purpose

The goal is to support consistent classification and handling of risks,
issues, actions, and decisions in regulated and cross-functional projects.

## Inputs

The triage logic evaluates:
- project phase
- functional area
- regulatory environment
- timeline impact
- deliverable impact
- whether the item is current
- whether a formal decision is needed
- whether the item is vendor-related
- primary function

## Outputs

The module returns:
- classification
- priority
- owner_type
- escalation_flag
- due_date_urgency
- rationale
- recommended_actions