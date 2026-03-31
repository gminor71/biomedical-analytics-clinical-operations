# Binary Method – Agent Logic

## Overview

This document defines a structured decision framework for selecting and
validating statistical methods for **binary endpoints** in clinical trials.

The goal is to translate method selection and analytical review into
**standardized, auditable logic** that supports:

* Consistent analytical decision-making
* Appropriate effect measure selection
* Integration into agent-assisted workflows

---

## Purpose

To:

* Identify appropriate statistical methods for binary endpoints
* Ensure design and data characteristics are evaluated and addressed
* Generate structured outputs for analysis planning and interpretation

---

## Inputs

* **Endpoint Definition**

  * Binary endpoint (for example, response / no response, event / no event)

* **Study Design**

  * Treatment groups
  * Covariates
  * Stratification factors

* **Data Characteristics**

  * Sample size
  * Event rate
  * Expected cell counts

---

## Trigger Conditions

Evaluate method selection when:

* A binary endpoint is defined in protocol or SAP
* Analysis planning phase (pre-analysis)
* Data characteristics suggest potential small-sample or sparse-data issues

---

## Decision Rules

### 1. Confirm Endpoint Type

* If endpoint is **binary** → proceed with binary method logic
* Otherwise → refer to alternative method logic

---

### 2. Baseline Method Selection

* Default primary method:

  * **Chi-square test** for group comparison

* Default effect measure:

  * **Risk difference**
  * **Risk ratio**

---

### 3. Assess Sample Size and Expected Cell Counts

* If sample size is adequate and expected cell counts are acceptable:

  * Retain chi-square test

* If sample size is small or expected cell counts are low:

  * Prefer **Fisher’s exact test**
  * Flag sparse-data concerns

---

### 4. Evaluate Covariate Adjustment

* If covariates are present:

  * Consider **logistic regression** for adjusted analysis
  * Use **odds ratio** as the primary effect measure for regression output

* If no covariates are present:

  * Unadjusted comparison may be sufficient depending on design

---

### 5. Assess Rare Event Risk

* If event rate is very low:

  * Flag possible instability in estimates
  * Consider exact or penalized approaches if standard methods are unstable

---

### 6. Interpretation Considerations

* If Fisher’s exact test is selected:

  * Emphasize exact inference for small-sample settings

* If logistic regression is suggested:

  * Review covariate relevance and interpret odds ratios carefully

* If rare events are present:

  * Highlight instability risk and cautious interpretation

---

## Classification

| Scenario                                | Recommended Approach                                  |
| --------------------------------------- | ----------------------------------------------------- |
| Standard binary endpoint                | Chi-square + risk difference / risk ratio             |
| Small sample or sparse expected counts  | Fisher’s exact test                                   |
| Covariates present                      | Logistic regression as adjusted method                |
| Rare event endpoint                     | Caution; consider exact or penalized methods if needed |

---

## Outputs

* **Recommended Method**
* **Effect Measure**
* **Alternative Method Suggestions**
* **Analysis Notes for SAP**
* **Interpretation Guidance**

---

## Escalation Criteria

Escalate for statistical review if:

* Sample size is very small
* Expected cell counts are sparse
* Rare events may destabilize estimates
* Covariate-adjusted and unadjusted results may differ meaningfully

---

## Human Review Requirements

* **Biostatistician**

  * Confirms method selection
  * Reviews effect measure suitability
  * Validates concerns related to sparse data or rare events

* **Clinical Team**

  * Reviews clinical relevance of endpoint and interpretation

---

## Example Scenario

**Study:**

* Oncology trial with objective response rate endpoint

**Data:**

* Binary endpoint
* Small sample size
* Low expected cell counts
* Baseline covariates available

**Agent Output:**

* Primary method: **Fisher’s exact test**
* Effect measure: **Odds ratio** if adjusted logistic regression is considered
* Recommendation:

  * Prefer Fisher’s exact test for unadjusted comparison
  * Consider logistic regression for adjusted analysis
  * Flag sparse-data concerns

---

## Relationship to Methods Library

This logic operationalizes:

* binary endpoint method selection guidance in the methods library
* broader statistical planning workflows in `agent-logic/biostatistics/`

---

## Key Principle

> Method selection for binary endpoint analysis should be systematic, 
design-aware, and transparently documented.

---

## Future Enhancements

* Expansion to repeated binary outcomes
* Direct linkage to SAP drafting and TFL planning
* Integration with exact and penalized modeling guidance
* Multi-agent interaction (Biostat + Clinical + Data Management)