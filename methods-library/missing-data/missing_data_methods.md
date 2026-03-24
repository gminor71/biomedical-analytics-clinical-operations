# Missing Data Methods & Considerations

## Overview

Missing data is a common challenge in clinical trials and can significantly 
impact the validity of statistical analyses.

Improper handling of missing data can lead to:

* Biased treatment effect estimates
* Reduced statistical power
* Misleading conclusions

This guide outlines types of missing data, appropriate handling methods, 
and key considerations for analysis and monitoring.

---

## Types of Missing Data

### 1. Missing Completely at Random (MCAR)

Missingness is unrelated to any observed or unobserved data.

**Example:**

* Data lost due to random technical issues

**Impact:**

* Minimal bias
* Reduced sample size

---

### 2. Missing at Random (MAR)

Missingness is related to observed variables but not the missing values themselves.

**Example:**

* Older patients more likely to miss visits

**Impact:**

* Can be addressed with appropriate statistical methods

---

### 3. Missing Not at Random (MNAR)

Missingness depends on unobserved data (the missing value itself).

**Example:**

* Patients with worsening symptoms drop out

**Impact:**

* High risk of bias
* Requires sensitivity analyses

---

## Common Missing Data Scenarios in Clinical Trials

* Patient dropout or loss to follow-up
* Missed visits or assessments
* Incomplete laboratory results
* Protocol deviations
* Early study termination

---

## Methods for Handling Missing Data

### 1. Complete Case Analysis

Analyze only subjects with complete data.

**Pros:**

* Simple

**Cons:**

* Loss of data
* Biased if not MCAR

---

### 2. Single Imputation Methods

#### Last Observation Carried Forward (LOCF)

Uses last observed value to fill missing data.

**Pros:**

* Simple

**Cons:**

* Can introduce bias
* Assumes no change over time

---

#### Mean/Median Imputation

Replaces missing values with summary statistics.

**Cons:**

* Underestimates variability
* Distorts relationships

---

### 3. Multiple Imputation (MI)

Creates multiple datasets with imputed values and combines results.

**Pros:**

* Accounts for uncertainty
* Appropriate under MAR

**Cons:**

* More complex
* Requires assumptions

---

### 4. Model-Based Methods

#### Mixed Models / Repeated Measures (MMRM)

* Uses all available data
* Handles missing data under MAR

---

#### Maximum Likelihood Methods

* Incorporates incomplete data into estimation

---

### 5. Sensitivity Analyses

Evaluate robustness of results under different assumptions.

**Examples:**

* Best-case / worst-case scenarios
* MNAR models

---

## Method Selection Considerations

| Scenario | Recommended Approach          |
| -------- | ----------------------------- |
| MCAR     | Complete case acceptable      |
| MAR      | Multiple imputation, MMRM     |
| MNAR     | Sensitivity analyses required |

---

## Red Flags

* High proportion of missing data
* Imbalance in missingness between treatment groups
* Missing data related to outcomes
* Use of LOCF without justification
* Lack of sensitivity analyses

---

## Common Pitfalls

### 1. Assuming Missing Data is Random

* Incorrect assumptions can bias results

---

### 2. Overuse of Simple Imputation

* Methods like LOCF can distort treatment effects

---

### 3. Ignoring Missing Data Patterns

* Patterns may reveal underlying bias

---

### 4. Lack of Transparency

* Missing data handling not clearly documented

---

## CRA & Clinical Operations Perspective

### What to Look For

* Are missing data rates reported and tracked?
* Is missingness balanced across treatment arms?
* Does the handling method align with the SAP?

---

### When to Escalate

* High dropout rates in one treatment group
* Missing data linked to adverse events or lack of efficacy
* Use of inappropriate imputation methods
* Lack of justification for missing data approach

---

### Query Triggers

* Request clarification on missing data handling method
* Verify consistency with Statistical Analysis Plan (SAP)
* Request sensitivity analyses if bias suspected
* Confirm completeness of critical endpoints

---

## Relationship to Project Work

**CT01 – Binary Endpoint Trial Analysis**

* Missing outcome data can impact logistic regression results

**CT02 – Time-to-Event Survival Analysis**

* Censoring and dropout affect survival estimates
* Non-informative censoring assumption must be evaluated

This guide provides the framework for evaluating how missing data impacts 
both analyses.

---

## Key Takeaway

> The way missing data is handled can have as much impact on results as the statistical model itself.

---
