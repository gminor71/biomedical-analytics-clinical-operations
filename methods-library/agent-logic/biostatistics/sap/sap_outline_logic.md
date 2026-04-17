# SAP Outline – Agent Logic

## Overview

This document defines a structured decision framework for drafting a
first-pass **Statistical Analysis Plan (SAP) outline** for clinical trial
endpoints.

The goal is to translate early-stage statistical planning into
**standardized, auditable logic** that supports:

* Consistent SAP development
* Assumption-aware analysis planning
* Integration into agent-assisted workflows

---

## Purpose

To:

* Define the core components of an SAP outline
* Ensure analysis planning elements are identified and structured
* Generate standardized outputs for SAP drafting and review

---

## Inputs

* **Study Design**

  * Trial design (for example, randomized controlled trial)
  * Treatment groups
  * Stratification factors

* **Endpoint Information**

  * Endpoint type
  * Endpoint name
  * Analysis objective

* **Estimand-Related Information**

  * Estimand strategy
  * Intercurrent events
  * Analysis population

* **Analysis Planning Factors**

  * Covariates
  * Missing data risk
  * Censoring concern
  * Assumption status (if relevant, such as PH assumption for TTE)

---

## Trigger Conditions

Evaluate SAP outline logic when:

* A protocol or endpoint requires statistical analysis planning
* A first-pass SAP structure is needed
* Analysis design elements must be reviewed before detailed modeling

---

## Decision Rules

### 1. Confirm Endpoint Type

* If endpoint type is supported → proceed with SAP outline logic
* Otherwise → refer to alternative SAP logic or method framework

---

### 2. Define Analysis Objective

* If objective is provided → use as submitted
* Otherwise → apply standardized default wording based on endpoint

---

### 3. Define Estimand

* If estimand strategy is specified → include in SAP outline
* If estimand strategy is incomplete or absent:

  * Flag for follow-up
  * Request clarification of intercurrent event handling

---

### 4. Define Analysis Population

* If population is specified → include directly
* If not specified:

  * Flag as missing
  * Escalate for statistical review

---

### 5. Define Endpoint Description

* Translate endpoint name and type into standardized SAP language
* Ensure endpoint is clearly framed for analysis

---

### 6. Assign Primary Method

* If endpoint is **time-to-event**:

  * **Kaplan-Meier (KM)** for estimation
  * **Log-rank test** for group comparison
  * **Cox Proportional Hazards (PH) model** for regression

* If method selection is uncertain or unsupported:

  * Flag for follow-up
  * Route to method-specific logic

---

### 7. Define Covariate Strategy

* If baseline covariates are provided:

  * Include as candidate adjustment variables
  * Note need to confirm clinical/statistical relevance

* If no covariates are provided:

  * Record absence
  * Determine whether unadjusted analysis is appropriate

---

### 8. Define Stratification

* If stratification factors are provided:

  * Include in SAP outline
  * Assess relevance to randomization and analysis

* If none are provided:

  * Record absence
  * Confirm whether stratified analysis is required

---

### 9. Assess Missing Data and Censoring

* If missing data risk is low:

  * Note expected limited impact

* If missing data risk is moderate or high:

  * Flag for additional review
  * Recommend sensitivity analysis planning

* If censoring is potentially informative:

  * Flag for sensitivity analysis
  * Highlight risk to interpretation

---

### 10. Plan Sensitivity Analyses

Sensitivity analyses should be considered when:

* Assumptions are uncertain or violated
* Missing data risk is elevated
* Informative censoring is suspected
* Estimand handling requires alternative scenarios

Examples:

* PH diagnostics and alternative survival methods
* Informative censoring sensitivity analyses
* Alternative estimand handling scenarios

---

### 11. Identify Follow-Up Items

Generate follow-up items when:

* Estimand components are incomplete
* Analysis population is missing
* Intercurrent event handling is unclear
* Covariate or stratification strategy is not specified
* Assumption checks are required before final method confirmation

---

## Classification

| Scenario                                  | Recommended Approach                                      |
| ----------------------------------------- | --------------------------------------------------------- |
| Well-specified TTE endpoint               | Draft full SAP outline with KM + Log-rank + Cox PH        |
| Estimand incomplete                       | Include follow-up item for estimand clarification         |
| Missing population definition             | Flag for statistical review                               |
| Informative censoring suspected           | Include sensitivity analysis planning                     |
| PH assumption unknown or potentially weak | Add diagnostic follow-up and alternative method planning  |

---

## Outputs

* **Analysis Objective**
* **Estimand Statement**
* **Analysis Population**
* **Endpoint Definition**
* **Primary Method**
* **Covariate Strategy**
* **Stratification Plan**
* **Missing Data Handling Notes**
* **Sensitivity Analysis Recommendations**
* **Follow-Up Items**
* **Warnings and Notes**

---

## Escalation Criteria

Escalate for statistical review if:

* Estimand is incomplete or unclear
* Analysis population is missing
* Informative censoring is suspected
* Assumptions for the primary method are uncertain or violated
* Key planning inputs are absent

---

## Human Review Requirements

* **Biostatistician**

  * Confirms SAP outline structure
  * Validates method appropriateness
  * Reviews missing components and follow-up items

* **Clinical Team**

  * Reviews endpoint relevance
  * Confirms treatment and intercurrent event context

---

## Example Scenario

**Study:**

* Oncology trial with overall survival endpoint

**Inputs:**

* Time-to-event endpoint
* ITT population
* Treatment policy estimand
* PH assumption unknown
* Moderate missing data risk

**Agent Output:**

* Draft SAP outline with:
  * Objective
  * Estimand
  * Analysis population
  * Primary method: KM + Log-rank + Cox PH
* Sensitivity analysis recommendation:

  * Assess PH assumption
  * Consider RMST if needed
* Follow-up item:

  * Confirm PH diagnostics before final method confirmation

---

## Relationship to Methods Library

This logic operationalizes:

* `time_to_event_method_selection.md` → endpoint-specific method selection
* `cox_ph_assumptions.md` → survival model assumption review
* future SAP planning references in `agent-logic/biostatistics/`

---

## Key Principle

> SAP drafting should be systematic, assumption-aware, and transparently documented so that statistical planning is reproducible before analysis begins.

---

## Future Enhancements

* Expansion to binary and continuous endpoint SAP logic
* Direct linkage to method-specific agent outputs
* Automated prompts for missing estimand components
* Integration with TFL planning and QC agents
* Multi-agent interaction (Biostat + Clinical + Regulatory)