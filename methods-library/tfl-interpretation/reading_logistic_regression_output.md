# How to Read Logistic Regression Output

## Overview

Logistic regression is used in clinical trials to analyze **binary outcomes**, 
such as:

* Response vs non-response
* Event vs no event
* Success vs failure

Results are typically reported as:

* **Odds Ratios (ORs)**
* **Confidence Intervals (CIs)**
* **P-values**

This guide explains how to interpret these outputs and identify potential issues.

---

## Key Components of Logistic Regression Output

### 1. Odds Ratio (OR)

Represents the relative odds of the outcome occurring in one group compared 
to another.

**Interpretation:**

* OR = 1 → no difference
* OR > 1 → higher odds of event
* OR < 1 → lower odds of event

---

### 2. Confidence Interval (CI)

Provides a range of plausible values for the odds ratio.

**Interpretation:**

* CI includes 1 → not statistically significant
* CI excludes 1 → statistically significant
* Wide CI → imprecise estimate

---

### 3. P-Value

Tests the null hypothesis (no association between predictor and outcome).

**Interpretation:**

* p < 0.05 → statistically significant
* p ≥ 0.05 → not statistically significant

---

### 4. Coefficients (Log-Odds)

Model coefficients are in log-odds form.

**Note:**

* Exponentiating coefficients → odds ratios

---

### 5. Model Fit & Performance Metrics

#### A. Hosmer-Lemeshow Test

* Assesses agreement between observed and predicted outcomes

#### B. ROC Curve / AUC

* Measures model discrimination ability

---

## How to Interpret Results

### 1. Direction of Effect

* OR > 1 → increased likelihood of outcome
* OR < 1 → decreased likelihood

---

### 2. Magnitude of Effect

* How far is OR from 1?
* Larger deviations → stronger effect

---

### 3. Precision of Estimate

* Narrow CI → more reliable
* Wide CI → uncertainty

---

### 4. Statistical Significance

* Does CI exclude 1?
* Is p-value below threshold?

---

### 5. Clinical Relevance

* Is the effect size meaningful in practice?
* Small OR differences may not be clinically important

---

## Example Interpretation

**Result:**

* OR = 0.65
* 95% CI: (0.45, 0.92)
* p = 0.02

**Interpretation:**

* 35% reduction in odds of the event
* Statistically significant (CI excludes 1)
* Likely meaningful effect, depending on context

---

## Red Flags

* Very wide confidence intervals
* Extremely large or small ORs (instability)
* Significant p-value with negligible effect size
* Non-significant result with large effect but low power
* Inconsistency between raw data and modeled results

---

## Common Pitfalls

### 1. Confusing Odds Ratio with Risk Ratio

* OR can overstate effect when event rates are high

---

### 2. Ignoring Baseline Event Rates

* Same OR can imply very different absolute risks

---

### 3. Overreliance on P-Values

* Statistical significance ≠ clinical importance

---

### 4. Ignoring Model Assumptions

* Violations can invalidate interpretation

---

## CRA & Clinical Operations Perspective

### What to Look For

* Do odds ratios align with raw event rates?
* Are covariates consistent with the SAP?
* Are results clinically meaningful?

---

### When to Escalate

* Large OR with very wide CI
* Model results contradict raw proportions
* Unexpected covariates included in model
* Lack of clarity on model specification

---

### Query Triggers

* Request raw event rate comparison
* Confirm covariate selection and justification
* Verify consistency with Statistical Analysis Plan (SAP)
* Request sensitivity analyses if results appear unstable

---

## Relationship to Project Work

**CT01 – Binary Endpoint Trial Analysis**

* Logistic regression model
* Odds ratio estimation
* Covariate-adjusted analysis

This guide supports interpretation of binary endpoint outputs generated in CT01.

---

## Key Takeaway

> Logistic regression outputs must be interpreted in context—odds ratios, confidence intervals, and clinical relevance should always be considered together.

---
