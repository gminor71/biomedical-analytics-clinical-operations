# Site Risk Scoring – Agent Logic

## Overview

This document defines a structured decision framework for evaluating 
and scoring **clinical trial site risk** based on data quality, 
operational performance, and protocol adherence.

The goal is to translate monitoring signals into
**standardized, auditable risk scores** that support:

* Risk-based monitoring (RBM)
* Prioritization of CRA activities
* Early detection of site-level issues
* Agent-assisted oversight workflows

---

## Purpose

To:

* Aggregate multiple site-level signals into a unified risk score
* Classify sites based on operational and data risk
* Trigger appropriate monitoring actions

---

## Inputs

### 1. Query Metrics

* Number of open queries
* Query age (average and maximum)
* Overdue queries (>14 days)

---

### 2. Data Quality

* Missing critical data (labs, endpoints, safety variables)
* Data entry timeliness
* Inconsistent or corrected data frequency

---

### 3. Protocol Compliance

* Number of protocol deviations
* Severity of deviations (minor vs major)
* Repeated deviations across visits or subjects

---

### 4. Operational Performance

* Enrollment rate vs expected
* Visit adherence (missed or delayed visits)
* Site responsiveness to queries

---

### 5. Historical Trends

* Issues identified in prior monitoring visits (MV1, MV2)
* Resolution time for past queries
* Recurrence of similar findings

---

## Trigger Conditions

Risk scoring is performed:

* On a scheduled basis (e.g., weekly)
* Prior to monitoring visits
* Following significant data updates

---

## Scoring Framework

Each domain contributes to an overall **Site Risk Score (0–100)**.

### 1. Query Burden (0–25)

* Low (<10 queries, minimal aging) → 5
* Moderate (10–20 queries or some overdue) → 15
* High (>20 queries or many overdue) → 25

---

### 2. Data Quality (0–25)

* Minimal missing data → 5
* Moderate missing or delayed entry → 15
* Critical missing data (safety/endpoints) → 25

---

### 3. Protocol Compliance (0–20)

* Few minor deviations → 5
* Multiple deviations → 10
* Major or repeated deviations → 20

---

### 4. Operational Performance (0–15)

* On track → 5
* Minor delays → 10
* Significant delays or poor responsiveness → 15

---

### 5. Historical Risk (0–15)

* No prior issues → 5
* Some prior issues resolved → 10
* Repeated or unresolved issues → 15

---

## Total Risk Score

**Site Risk Score = Sum of all domain scores (0–100)**

---

## Risk Classification

| Score Range | Risk Level | Description                                      |
| ----------- | ---------- | ------------------------------------------------ |
| 0–25        | Low        | Stable site, minimal oversight required          |
| 26–50       | Moderate   | Some issues requiring monitoring attention       |
| 51–75       | High       | Significant concerns requiring active management |
| 76–100      | Critical   | Immediate intervention required                  |

---

## Decision Rules

* If **High or Critical Risk**:

  * Prioritize for monitoring (remote or onsite)
  * Increase monitoring frequency

* If **Moderate Risk**:

  * Track trends
  * Address during routine monitoring

* If **Low Risk**:

  * Maintain standard monitoring schedule

---

## Outputs

* **Site Risk Score (numeric)**
* **Risk Classification (Low / Moderate / High / Critical)**
* **Contributing Factors (by domain)**
* **Recommended Monitoring Actions**

---

## Recommended Actions

### Low Risk

* Routine monitoring
* No escalation

### Moderate Risk

* Targeted follow-up on identified issues
* Monitor for trend changes

### High Risk

* Immediate CRA review
* Increased monitoring frequency
* Add to monitoring report

### Critical Risk

* Escalate to study team
* Consider onsite visit or intervention
* Document in risk review

---

## Escalation Criteria

Escalate if:

* Critical data missing
* Persistent non-response from site
* Repeated major protocol deviations
* Risk score remains High/Critical across multiple cycles

---

## Human Review Requirements

* **CRA**

  * Validates risk score and recommended actions

* **Study Team / Sponsor**

  * Reviews high or critical risk sites
  * Determines intervention strategy

---

## Example Scenario

**Site B:**

* 22 open queries (10 overdue)
* Missing key lab data
* Multiple protocol deviations (including repeat issues)
* Delayed visit documentation

**Scoring:**

* Query Burden: 25
* Data Quality: 25
* Protocol Compliance: 20
* Operational Performance: 10
* Historical Risk: 15

**Total Score: 95 → Critical Risk**

**Recommended Actions:**

* Immediate escalation
* Onsite monitoring consideration
* Sponsor notification

---

## Relationship to Other Agent Logic

This logic integrates with:

* `cra_query_triage.md` → issue detection and prioritization
* `protocol_deviation_classification.md` → deviation severity
* Monitoring workflows (MV1 / MV2) → operational context

---

## Key Principle

> Site risk should be continuously assessed using multiple signals, not based on isolated findings.

---

## Future Enhancements

* Dynamic weighting of domains based on study phase
* Integration with centralized monitoring dashboards
* Predictive risk modeling
* Multi-agent coordination across clinical operations and data management

---
