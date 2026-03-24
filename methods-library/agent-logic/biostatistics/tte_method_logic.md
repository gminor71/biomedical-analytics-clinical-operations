# Time-to-Event Method – Agent Logic

## Overview

This document defines a structured decision framework for selecting and
validating statistical methods for **time-to-event (TTE)** endpoints in 
clinical trials.

The goal is to translate method selection and diagnostic evaluation 
into **standardized, auditable logic** that supports:

* Consistent analytical decision-making
* Assumption validation
* Integration into agent-assisted workflows

---

## Purpose

To:

* Identify appropriate statistical methods for TTE endpoints
* Ensure model assumptions are evaluated and addressed
* Generate structured outputs for analysis planning and interpretation

---

## Inputs

* **Endpoint Definition**

  * Time-to-event (e.g., overall survival, progression-free survival)

* **Censoring Information**

  * Presence and proportion of censored observations

* **Study Design**

  * Treatment groups
  * Covariates
  * Stratification factors

* **Data Characteristics**

  * Sample size
  * Event rate
  * Follow-up duration

---

## Trigger Conditions

Evaluate method selection when:

* A TTE endpoint is defined in protocol or SAP
* Analysis planning phase (pre-analysis)
* Model diagnostics indicate assumption violations

---

## Decision Rules

### 1. Confirm Endpoint Type

* If endpoint is **time-to-event** → proceed with TTE methods
* Otherwise → refer to alternative method logic

---

### 2. Baseline Method Selection

* Default primary method:

  * **Kaplan-Meier (KM)** for estimation
  * **Log-rank test** for group comparison
  * **Cox Proportional Hazards (PH) model** for regression

---

### 3. Assess Censoring

* If censoring is present and non-informative → proceed
* If censoring is potentially informative → flag for sensitivity analysis

---

### 4. Evaluate Proportional Hazards (PH) Assumption

Use:

* Schoenfeld residuals
* Log(-log) survival plots

#### Decision:

* If PH assumption **holds** → retain Cox PH model
* If PH assumption **violated** → proceed to alternative modeling

---

### 5. Alternative Methods (if PH violated)

Consider:

* Stratified Cox model
* Time-varying covariates
* Accelerated Failure Time (AFT) models
* Restricted Mean Survival Time (RMST)

---

### 6. Sample Size and Event Considerations

* If **low event rate or small sample size**:

  * Flag reduced statistical power
  * Emphasize descriptive KM interpretation

---

### 7. Covariate Adjustment

* If covariates present:

  * Include in Cox model (if appropriate)
  * Assess for confounding and interaction effects

---

## Classification

| Scenario                        | Recommended Approach                           |
| ------------------------------- | ---------------------------------------------- |
| Standard TTE + PH holds         | KM + Log-rank + Cox PH                         |
| PH violated                     | Alternative models (AFT, RMST, stratified Cox) |
| Low events                      | Descriptive KM + caution in inference          |
| Informative censoring suspected | Sensitivity analyses required                  |

---

## Outputs

* **Recommended Method**
* **Assumption Status (PH: met / violated)**
* **Alternative Method Suggestions (if applicable)**
* **Analysis Notes for SAP**
* **Interpretation Guidance**

---

## Escalation Criteria

Escalate for statistical review if:

* PH assumption is violated
* Informative censoring is suspected
* Model instability due to small sample size or low events

---

## Human Review Requirements

* **Biostatistician**

  * Confirms method selection
  * Validates model assumptions

* **Clinical Team**

  * Reviews clinical relevance of endpoint and interpretation

---

## Example Scenario

**Study:**

* Oncology trial with progression-free survival endpoint

**Data:**

* Moderate censoring
* PH assumption violated based on Schoenfeld residuals

**Agent Output:**

* Primary method: KM + Cox PH (initial)
* PH status: **Violated**
* Recommendation:

  * Consider RMST or time-varying Cox model
  * Include sensitivity analysis

---

## Relationship to Methods Library

This logic operationalizes:

* `time_to_event_method_selection.md` → method selection
* `cox_ph_assumptions.md` → assumption validation
* `reading_kaplan_meier_curve.md` → interpretation

---

## Key Principle

> Method selection and validation for time-to-event analysis should be systematic, assumption-driven, and transparently documented.

---

## Future Enhancements

* Integration with automated diagnostic outputs
* Direct linkage to TFL generation
* Cross-validation with trial-level risk metrics
* Multi-agent interaction (Biostat + Clinical + Data Management)

---
