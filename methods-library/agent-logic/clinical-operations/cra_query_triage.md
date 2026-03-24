# CRA Query Triage – Agent Logic

## Overview

This document defines a structured decision framework for identifying, 
prioritizing, and managing site-level data issues requiring Clinical Research Associate (CRA) follow-up.

The goal is to translate query management workflows into
**standardized, auditable decision logic** that can support:

* Consistent monitoring practices
* Risk-based prioritization
* Automation and agent-assisted workflows

---

## Purpose

To:

* Detect overdue, missing, or inconsistent data
* Prioritize issues based on risk and recurrence
* Generate actionable outputs for CRA monitoring activities

---

## Inputs

Primary data sources:

* **Query Log**

  * Open queries
  * Query age
  * Query type (data clarification, missing data, etc.)

* **Visit Data**

  * Visit dates
  * Expected data collection windows

* **Missing Data Reports**

  * Labs
  * Endpoints
  * Key safety variables

* **Protocol Requirements**

  * Critical data definitions
  * Visit schedules
  * Data timelines

---

## Trigger Conditions

The agent evaluates data when any of the following occur:

* New data entry or query creation
* Scheduled monitoring review (e.g., weekly)
* Pre-visit review (MV preparation)

---

## Decision Rules

### 1. Query Aging

* If **query age > 14 days** → classify as *Overdue*
* If **query age > 30 days** → classify as *Critical Overdue*

---

### 2. Query Volume (Site-Level)

* If **open queries > 10** → flag *Elevated Query Burden*
* If **open queries > 20** → flag *High Query Burden*

---

### 3. Missing Critical Data

* If **critical lab or endpoint data missing > 7 days** → flag *Missing Critical Data*

---

### 4. Repeat Issues

* If the same issue type occurs across:

  * Multiple visits, or
  * Multiple subjects

→ classify as *Repeated Issue*

---

### 5. Combined Risk Escalation

* If **Overdue queries + Missing Critical Data** → escalate to *High Priority*
* If **Repeated Issues + High Query Burden** → escalate to *Site Risk Review*

---

## Classification

| Level  | Description                                                   |
| ------ | ------------------------------------------------------------- |
| Low    | Isolated query, within acceptable timelines                   |
| Medium | Multiple overdue queries or minor missing data                |
| High   | Critical data missing, repeated issues, or significant delays |

---

## Actions

Based on classification:

### Low

* Monitor
* No immediate escalation

### Medium

* Generate or follow up on queries
* Add to CRA action log
* Address during next monitoring activity

### High

* Prioritize for immediate CRA follow-up
* Draft monitoring report language
* Escalate to study team if required

---

## Outputs

* **Prioritized Issue List**
* **CRA Action Log Entries**
* **Suggested Query Text (optional)**
* **Draft Monitoring Report Language**

---

## Escalation Criteria

Escalate to study team if:

* Critical safety data is missing
* Site is non-responsive to queries
* Repeated protocol-related issues persist

---

## Human Review Requirements

All outputs require validation by:

* **CRA**

  * Confirms findings and actions

* **Data Management**

  * Reviews query content and resolution

---

## Example Scenario

**Site A:**

* 15 open queries
* 6 queries > 14 days old
* Missing lab data for key endpoint (10 days overdue)

**Agent Classification:**

* High Query Burden
* Overdue Queries
* Missing Critical Data

**Result:**

* Classification: **High Priority**
* Actions:

  * Immediate follow-up required
  * Add to monitoring report
  * Escalate if unresolved

---

## Relationship to Monitoring Workflow

This logic supports:

* Pre-monitoring visit review
* Remote monitoring activities
* Follow-up from prior visits (MV1 → MV2 continuity)

---

## Key Principle

> Query management should be proactive, risk-based, and consistent across sites—not reactive or ad hoc.

---

## Future Enhancements

* Integration with site risk scoring models
* Automated query generation
* Linkage to protocol deviation classification
* Multi-agent coordination (CRA + Data Management)

---
