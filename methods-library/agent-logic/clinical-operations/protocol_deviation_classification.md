# Protocol Deviation Classification – Agent Logic

## Overview

This document defines a structured decision framework for identifying,
classifying, and prioritizing **protocol deviations** in clinical trials.

The goal is to standardize how deviations are assessed for:

* Severity
* Impact on subject safety and data integrity
* Required follow-up actions

This logic supports:

* Consistent deviation handling
* Risk-based monitoring (RBM)
* Integration into agent-assisted workflows

---

## Purpose

To:

* Classify protocol deviations based on severity and impact
* Identify patterns of repeated or systemic issues
* Trigger appropriate escalation and documentation

---

## Inputs

### 1. Deviation Details

* Description of deviation
* Timing (relative to visit schedule)
* Affected subject(s)

---

### 2. Protocol Requirements

* Inclusion / exclusion criteria
* Visit schedule
* Required procedures
* Endpoint definitions

---

### 3. Data Impact

* Missing or incorrect data
* Impact on primary or secondary endpoints

---

### 4. Safety Impact

* Impact on subject safety
* Missed safety assessments
* Dosing errors

---

### 5. Frequency and Pattern

* Single occurrence vs repeated issue
* Occurrence across multiple subjects or visits

---

## Trigger Conditions

Deviation classification is performed when:

* A deviation is identified during monitoring
* Data review identifies inconsistency with protocol
* Site reports a protocol deviation

---

## Decision Rules

### 1. Identify Deviation Type

Common categories:

* Eligibility deviation
* Visit schedule deviation
* Missed or incomplete procedure
* Dosing deviation
* Data recording error

---

### 2. Assess Safety Impact

* If deviation affects subject safety → classify as **Major**
* If no safety impact → proceed to data impact assessment

---

### 3. Assess Data Integrity Impact

* If deviation affects:

  * Primary endpoint
  * Key secondary endpoints

→ classify as **Major**

* If minor or no impact on endpoints → proceed

---

### 4. Assess Regulatory/Protocol Compliance

* If deviation violates:

  * Inclusion/exclusion criteria
  * Informed consent process

→ classify as **Major**

---

### 5. Assess Frequency

* If deviation:

  * Repeats across subjects
  * Occurs across multiple visits

→ escalate classification level

---

## Classification

| Level    | Description                                                                |
| -------- | -------------------------------------------------------------------------- |
| Minor    | No significant impact on safety or data integrity                          |
| Major    | Potential or confirmed impact on safety, endpoints, or protocol compliance |
| Critical | Significant safety risk, major eligibility violation, or systemic issue    |

---

## Escalation Rules

* If **Major deviation + repeated occurrence** → escalate to **Critical**
* If **Critical deviation identified** → immediate escalation required

---

## Outputs

* **Deviation Classification (Minor / Major / Critical)**
* **Deviation Type**
* **Impact Summary (Safety / Data / Compliance)**
* **Recommended Actions**

---

## Recommended Actions

### Minor

* Document deviation
* Monitor for recurrence

---

### Major

* Document and report deviation
* Open query (if applicable)
* Include in monitoring report
* Follow up with site

---

### Critical

* Immediate escalation to study team
* Document in deviation log
* Consider corrective and preventive actions (CAPA)
* Evaluate site risk impact

---

## Escalation Criteria

Escalate if:

* Safety risk identified
* Eligibility violation occurs
* Deviation affects primary endpoint
* Repeated deviations suggest systemic issue

---

## Human Review Requirements

* **CRA**

  * Confirms classification
  * Documents in monitoring report

* **Medical Monitor**

  * Reviews safety-related deviations

* **Sponsor / Study Team**

  * Reviews major and critical deviations
  * Determines CAPA

---

## Example Scenario

**Deviation:**

* Subject enrolled despite not meeting inclusion criteria

**Assessment:**

* Protocol violation (eligibility)
* Potential impact on study population validity

**Classification:**

* **Major → escalated to Critical** (due to severity)

**Actions:**

* Immediate escalation
* Document deviation
* Assess impact on analysis population

---

## Relationship to Other Agent Logic

This logic integrates with:

* `cra_query_triage.md` → identifies related data issues
* `site_risk_scoring.md` → contributes to risk score
* Monitoring workflows (MV1 / MV2) → operational context

---

## Key Principle

> Protocol deviations should be evaluated based on impact—not just occurrence—and handled consistently across sites.

---

## Future Enhancements

* Automated deviation detection from EDC data
* Integration with CAPA tracking systems
* Linkage to statistical analysis populations (e.g., ITT, PP)
* Cross-functional escalation workflows

---
