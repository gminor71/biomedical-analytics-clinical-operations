# Common Statistical Pitfalls in Clinical Trials

## Overview

Clinical trial analyses require careful alignment between statistical methods,
study design, and clinical interpretation.

Missteps in statistical analysis can lead to:

* Incorrect conclusions about treatment efficacy
* Misinterpretation of results
* Increased regulatory and audit risk

This guide highlights common statistical pitfalls and how to identify them.

---

## 1. Misalignment with the Statistical Analysis Plan (SAP)

### Issue

Analysis methods, populations, or endpoints differ from those pre-specified 
in the SAP.

### Examples

* Using a different analysis population (e.g., per-protocol vs ITT)
* Changing primary endpoints post hoc
* Adding unplanned covariates

### Impact

* Introduces bias
* Undermines trial integrity
* Raises regulatory concerns

### CRA Insight

Verify that all analyses align with SAP definitions and pre-specified methods.

---

## 2. Misinterpreting Statistical Significance

### Issue

Over reliance on p-values without considering effect size or clinical relevance.

### Example

* p < 0.05 but treatment effect is minimal

### Impact

* Overstating treatment benefit
* Misleading clinical conclusions

### Key Principle

Statistical significance ≠ clinical importance

---

## 3. Ignoring Model Assumptions

### Issue

Statistical models are applied without verifying assumptions.

### Examples

* Cox model used despite non-proportional hazards
* Logistic regression with non-linear predictors

### Impact

* Biased or invalid estimates
* Misleading interpretation

### CRA Insight

Ensure diagnostics (e.g., Schoenfeld residuals) are performed and reported.

---

## 4. Inappropriate Method Selection

### Issue

Using statistical methods that do not match the endpoint or data structure.

### Examples

* Using linear regression for binary outcomes
* Ignoring censoring in time-to-event data

### Impact

* Invalid results
* Incorrect treatment effect estimates

---

## 5. Misinterpreting Effect Measures

### Issue

Confusion between different measures of effect.

### Examples

* Interpreting odds ratios as risk ratios
* Overinterpreting hazard ratios without context

### Impact

* Over- or underestimation of treatment effect

---

## 6. Overfitting Models

### Issue

Including too many predictors relative to the number of events.

### Example

* Logistic regression with limited outcome events

### Impact

* Unstable estimates
* Poor generalizability

---

## 7. Ignoring Missing Data Mechanisms

### Issue

Missing data handled without understanding underlying mechanism.

### Examples

* Complete case analysis when data is not MCAR
* Use of LOCF without justification

### Impact

* Biased results
* Reduced validity

### CRA Insight

Assess whether missing data handling aligns with SAP and study design.

---

## 8. Overreliance on Subgroup Analyses

### Issue

Drawing strong conclusions from exploratory subgroup results.

### Examples

* Multiple subgroup comparisons without adjustment
* Highlighting favorable subgroup findings

### Impact

* False-positive findings
* Misleading interpretation

---

## 9. Multiplicity Not Addressed

### Issue

Multiple comparisons performed without adjustment.

### Examples

* Multiple endpoints or time points tested
* No correction for multiple testing

### Impact

* Inflated Type I error rate
* Increased chance of false-positive results

---

## 10. Disconnect Between Raw Data and Model Results

### Issue

Model outputs contradict observed data patterns.

### Examples

* Kaplan-Meier curves show minimal difference but HR suggests strong effect
* Raw proportions differ from modeled odds ratios

### Impact

* Signals model instability or misspecification

### CRA Insight

Always compare model results with raw data summaries.

---

## 11. Poor Interpretation of Censoring (Time-to-Event)

### Issue

Censoring patterns not considered in interpretation.

### Examples

* Heavy or imbalanced censoring across groups
* Ignoring loss to follow-up

### Impact

* Distorted survival estimates
* Misleading conclusions

---

## 12. Lack of Sensitivity Analyses

### Issue

Results presented without assessing robustness.

### Examples

* No alternative models tested
* No evaluation of assumption violations

### Impact

* Reduced confidence in findings
* Increased regulatory scrutiny

---

## CRA & Clinical Operations Perspective

### What to Look For

* Alignment with SAP
* Consistency between raw data and modeled results
* Presence of diagnostics and sensitivity analyses
* Appropriate handling of missing data

---

### When to Escalate

* Deviations from SAP without justification
* Model assumptions clearly violated
* Results driven by small samples or unstable estimates
* Inconsistencies between different analyses

---

### Query Triggers

* Request clarification on method selection
* Confirm model diagnostics were performed
* Verify handling of missing data
* Request sensitivity analyses where appropriate

---

## Relationship to Project Work

**CT01 – Binary Endpoint Trial Analysis**

* Logistic regression modeling
* Odds ratio interpretation

**CT02 – Time-to-Event Survival Analysis**

* Kaplan-Meier estimation
* Cox proportional hazards modeling
* Model diagnostics

This guide highlights risks that may arise in both project contexts.

---

## Key Takeaway

> Most statistical errors in clinical trials are not due to complex mathematics—they arise from misalignment, poor assumptions, and incorrect interpretation.

---
