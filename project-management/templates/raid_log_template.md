# RAID Log Template

This template supports structured tracking of **risks, actions, issues, and decisions**
across clinical research, analytics, and cross-functional project delivery.

## Purpose

Use this log to:
- track open and closed project items
- assign ownership and due dates
- support escalation and governance review
- maintain traceability across project phases

## Instructions

- Use one row per tracked item
- Set `type` as one of: `Risk`, `Action`, `Issue`, `Decision`
- Update status regularly during project reviews
- Include clear owners and due dates where applicable
- Use `source` to link the item to a meeting, visit, milestone, or workflow origin

## RAID Log

| id | type | title | description | impact | likelihood | priority | owner | status | created_date | due_date | source | phase | escalation_flag | notes |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| RAID-001 | Risk |  |  | High | Medium | High |  | Open |  |  |  | Planning | No |  |
| RAID-002 | Action |  |  |  |  | Medium |  | Open |  |  |  | Execution | No |  |
| RAID-003 | Issue |  |  | High |  | High |  | Open |  |  |  | Execution | Yes |  |
| RAID-004 | Decision |  |  |  |  | Medium |  | Closed |  |  |  | Planning | No |  |

## Field Definitions

- **id**: unique identifier for the item
- **type**: Risk, Action, Issue, or Decision
- **title**: short label for the item
- **description**: concise summary of the item
- **impact**: Low, Medium, High, or Critical where applicable
- **likelihood**: primarily used for risks
- **priority**: Low, Medium, High, or Critical
- **owner**: responsible individual or function
- **status**: Open, In Progress, Blocked, Closed
- **created_date**: date item was first logged
- **due_date**: target resolution or completion date
- **source**: originating meeting, site visit, tracker, milestone, or workflow
- **phase**: Planning, Execution, Monitoring, Closeout
- **escalation_flag**: Yes or No
- **notes**: additional context or updates

## Recommended Usage Notes

- **Risks** should include likelihood and impact
- **Issues** should focus on current problems requiring action
- **Actions** should be specific and time-bound
- **Decisions** should document the outcome and, if useful, cross-reference the decision log