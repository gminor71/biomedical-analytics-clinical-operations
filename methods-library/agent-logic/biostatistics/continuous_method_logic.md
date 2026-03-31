
# Continuous Method – Agent Logic

## Overview

This document defines a structured decision framework for selecting and
validating statistical methods for **continuous endpoints** in clinical trials.

The goal is to translate method selection and analytical review into
**standardized, auditable logic** that supports:

* Consistent analytical decision-making
* Appropriate effect measure selection
* Integration into agent-assisted workflows

---

## Purpose

To:

* Identify appropriate statistical methods for continuous endpoints
* Ensure distributional and variance assumptions are evaluated and addressed
* Generate structured outputs for analysis planning and interpretation

---

## Inputs

* **Endpoint Definition**

  * Continuous endpoint (for example, blood pressure change, biomarker level, quality of life score)

* **Study Design**

  * Number of groups
  * Covariates
  * Stratification factors

* **Data Characteristics**

  * Sample size
  * Distribution shape
  * Variance homogeneity

---

## Trigger Conditions

Evaluate method selection when:

* A continuous endpoint is defined in protocol or SAP
* Analysis planning phase (pre-analysis)
* Data characteristics suggest assumption concerns

---

## Decision Rules

### 1. Confirm Endpoint Type

* If endpoint is **continuous** → proceed with continuous method logic
* Otherwise → refer to alternative method logic

---

### 2. Baseline Method Selection

* If there are **two groups**:

  * **Independent t-test** for unadjusted comparison

* If there are **more than two groups**:

  * **ANOVA** for unadjusted comparison

* Default effect measure:

  * **Mean difference**

---

### 3. Assess Normality

* If distribution is approximately normal:

  * Retain standard parametric approach

* If distribution is non-normal:

  * Flag distributional concern
  * Consider nonparametric alternatives:
    * **Wilcoxon rank-sum test** for two groups
    * **Kruskal-Wallis test** for more than two groups

---

### 4. Assess Variance Homogeneity

* If variances are equal:

  * Retain standard t-test or ANOVA

* If variances are unequal and there are two groups:

  * Prefer **Welch t-test**

* If variances are unequal in multi-group settings:

  * Escalate for additional statistical review

---

### 5. Evaluate Covariate Adjustment

* If covariates are present:

  * Consider **linear regression** for adjusted analysis
  * Retain mean difference as core interpretation framework

* If no covariates are present:

  * Unadjusted comparison may be sufficient depending on design

---

### 6. Interpretation Considerations

* If parametric assumptions are reasonably supported:

  * Mean difference remains the primary effect measure

* If nonparametric alternatives are needed:

  * Interpretation should focus on distributional comparison rather than strict mean-based inference

* If regression is suggested:

  * Review covariate relevance and model assumptions

---

## Classification

| Scenario                                | Recommended Approach                              |
| --------------------------------------- | ------------------------------------------------- |
| Two-group continuous endpoint           | Independent t-test + mean difference              |
| More than two groups                    | ANOVA + mean difference                           |
| Non-normal distribution                 | Wilcoxon or Kruskal-Wallis as alternative         |
| Unequal variance (two groups)           | Welch t-test                                      |
| Covariates present                      | Linear regression as adjusted method              |

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

* Distributional assumptions are strongly violated
* Variance heterogeneity complicates inference
* Multi-group unequal variance requires more specialized handling
* Adjusted and unadjusted approaches may lead to materially different interpretation

---

## Human Review Requirements

* **Biostatistician**

  * Confirms method selection
  * Reviews assumptions and alternative methods
  * Validates covariate adjustment strategy

* **Clinical Team**

  * Reviews clinical relevance of endpoint and interpretation

---

## Example Scenario

**Study:**

* Cardiovascular trial with change in systolic blood pressure endpoint

**Data:**

* Two groups
* Approximately normal distribution
* Equal variance
* Baseline covariates available

**Agent Output:**

* Primary method: **Independent t-test**
* Effect measure: **Mean difference**
* Recommendation:

  * Consider linear regression for adjusted analysis
  * Retain mean difference for interpretation

---

## Relationship to Methods Library

This logic operationalizes:

* continuous endpoint method selection guidance in the methods library
* broader statistical planning workflows in `agent-logic/biostatistics/`

---

## Key Principle

> Method selection for continuous endpoint analysis should be systematic, assumption-aware, and transparently documented.

---

## Future Enhancements

* Expansion to repeated-measures and longitudinal continuous outcomes
* Direct linkage to SAP drafting and TFL planning
* Integration with transformation guidance
* Multi-agent interaction (Biostat + Clinical + Data Management)