# Binary Endpoint – Method Selection Guide

## Overview

Binary endpoints are commonly used in clinical trials to evaluate outcomes 
with two possible states, such as:

* Response vs non-response
* Event vs no event
* Success vs failure

Selection of the appropriate statistical method depends on:

* Study objective
* Sample size
* Need for covariate adjustment
* Data structure

This guide provides a framework for selecting appropriate methods for 
binary outcomes.

---

## Step 1 – Define the Objective

### A. Describe Outcome Proportions

* Goal: Estimate event rates within groups

→ **Use: Descriptive Statistics**

* Proportions
* Confidence intervals

---

### B. Compare Groups (Unadjusted)

* Goal: Test for differences between treatment arms

→ **Use:**

* Chi-Square Test (large sample sizes)
* Fisher’s Exact Test (small sample sizes)

---

### C. Estimate Treatment Effect (Unadjusted)

* Goal: Quantify association between treatment and outcome

→ **Use:**

* Risk Difference (RD)
* Risk Ratio (RR)
* Odds Ratio (OR)

---

### D. Adjust for Covariates

* Goal: Control for baseline differences and confounding

→ **Use: Logistic Regression**

---

### E. Estimate Relative Risk Directly

* Goal: Obtain interpretable risk ratios

→ **Use:**

* Log-binomial regression
* Poisson regression with robust variance

---

## Step 2 – Assess Key Data Characteristics

### 1. Sample Size

* Small samples may invalidate asymptotic tests

→ Use Fisher’s Exact Test

---

### 2. Event Rate

* Rare events vs common events impacts interpretation

→ OR approximates RR only when events are rare

---

### 3. Covariate Adjustment Needed?

* Important baseline differences present?

→ Use logistic regression or adjusted models

---

### 4. Independence of Observations

* Are observations independent?

→ If NO:

* Consider clustered or mixed-effects models

---

### 5. Missing Data

* Missing outcomes or covariates can bias results

→ Ensure appropriate handling (e.g., imputation)

---

## Method Summary

| Objective             | Method                 | Notes                  |
| --------------------- | ---------------------- | ---------------------- |
| Describe proportions  | Descriptive stats      | Basic summary          |
| Compare groups        | Chi-square / Fisher’s  | Unadjusted comparison  |
| Estimate effect       | RD / RR / OR           | Measure of association |
| Adjust for covariates | Logistic regression    | Most common approach   |
| Direct RR estimation  | Log-binomial / Poisson | Alternative to OR      |

---

## Red Flags in Method Selection

* Using Chi-square with very small sample sizes
* Reporting odds ratios when risk ratios are more appropriate
* Ignoring need for covariate adjustment
* Using logistic regression with insufficient events
* Misinterpreting OR as RR

---

## CRA & Clinical Operations Perspective

### What to Look For

* Does the method align with the endpoint definition in the SAP?
* Are unadjusted and adjusted analyses both presented (if required)?
* Are effect measures clearly defined and interpretable?

---

### When to Escalate

* Method does not match endpoint type
* Odds ratios presented without context for interpretation
* Lack of covariate adjustment when baseline imbalance exists
* Small sample size with inappropriate statistical test

---

### Query Triggers

* Request justification for selected statistical method
* Confirm covariate inclusion aligns with SAP
* Verify consistency between raw proportions and modeled results
* Clarify interpretation of effect measures (OR vs RR)

---

## Relationship to Project Work

**CT01 – Binary Endpoint Trial Analysis**

* Descriptive statistics (event rates)
* Logistic regression (adjusted analysis)
* Odds ratio estimation

This guide defines **how methods are selected**, while CT01 demonstrates
**how they are implemented**.

---

## Key Takeaway

> Method selection for binary endpoints depends on study objectives, data structure, and interpretability—not just the availability of statistical models.

---
