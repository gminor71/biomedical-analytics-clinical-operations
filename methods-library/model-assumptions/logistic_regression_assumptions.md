# Logistic Regression – Assumptions & Diagnostics

## Overview

Logistic regression is commonly used in clinical trials to analyze
**binary endpoints**, such as:

* Response vs non-response
* Event vs no event
* Success vs failure

It estimates the relationship between covariates and the probability of an 
outcome, typically reported as **odds ratios (ORs)**.

The validity of results depends on key assumptions. Violations can lead to:

* Biased effect estimates
* Misleading conclusions
* Incorrect clinical interpretation

---

## Core Assumptions

### 1. Correct Model Specification

The model includes all relevant variables and correctly represents their
relationships.

**Considerations:**

* Omitted variables can bias results
* Interaction terms may be required
* Non-linear relationships should be addressed

---

### 2. Independence of Observations

Each observation is independent of others.

**Risk Factors:**

* Repeated measures
* Clustered data (e.g., sites, centers)
* Multiple records per subject

---

### 3. Linearity of Continuous Predictors (Log-Odds Scale)

Continuous variables are linearly related to the log-odds of the outcome.

**Example:**

* Age or biomarker values may not have a linear relationship

---

### 4. Absence of Multicollinearity

Predictors are not highly correlated with each other.

**Impact:**

* Inflated standard errors
* Unstable coefficient estimates

---

### 5. Sufficient Sample Size / Events per Variable

Adequate number of outcome events relative to predictors.

**Rule of Thumb:**

* ~10 events per variable (context-dependent)

---

## Diagnostics & Evaluation

### 1. Linearity Assessment

* Plot continuous variables vs log-odds
* Use splines or transformations if needed

---

### 2. Multicollinearity Checks

* Variance Inflation Factor (VIF)
* Correlation matrices

**Interpretation:**

* High VIF (>5–10) suggests concern

---

### 3. Model Fit

#### Hosmer-Lemeshow Test

* Assesses agreement between observed and predicted outcomes

**Interpretation:**

* Non-significant result → acceptable fit
* Significant result → potential misfit

---

### 4. Discrimination

#### ROC Curve / AUC

* Measures ability to distinguish between outcome groups

**Interpretation:**

* AUC ~0.5 → no discrimination
* AUC ≥0.7 → acceptable
* AUC ≥0.8 → strong

---

### 5. Influence & Outliers

* Identify influential observations
* Assess leverage and residuals

---

## Red Flags

* Very large or unstable odds ratios
* Wide confidence intervals
* High multicollinearity between predictors
* Poor model fit (e.g., significant Hosmer-Lemeshow test)
* Small number of events relative to predictors

---

## Common Pitfalls

### 1. Interpreting Odds Ratios as Risk Ratios

* OR ≠ Risk Ratio, especially when event rates are high

---

### 2. Ignoring Non-Linearity

* Continuous predictors may distort results if not properly modeled

---

### 3. Overfitting the Model

* Too many variables relative to sample size

---

### 4. Ignoring Missing Data

* Can bias estimates if not handled appropriately

---

## Actions if Assumptions Are Violated

### 1. Address Non-Linearity

* Use transformations (log, polynomial)
* Apply spline models

---

### 2. Reduce Multicollinearity

* Remove or combine correlated variables
* Use dimension reduction techniques

---

### 3. Adjust for Clustering

* Use mixed-effects models
* Apply robust standard errors

---

### 4. Simplify the Model

* Reduce number of predictors
* Focus on clinically relevant variables

---

## CRA & Clinical Operations Perspective

### What to Look For

* Does the model align with the endpoint defined in the SAP?
* Are covariates pre-specified and justified?
* Are odds ratios clinically interpretable?

---

### When to Escalate

* Model includes unexpected or post-hoc covariates
* Results driven by small sample size or sparse events
* Large discrepancies between raw proportions and modeled results
* Lack of model diagnostics or justification

---

### Query Triggers

* Request clarification on covariate selection
* Confirm model specification matches SAP
* Verify handling of missing data
* Request sensitivity analyses if instability suspected

---

## Relationship to Project Work

**CT01 – Binary Endpoint Trial Analysis**

* Logistic regression model
* Odds ratio estimation
* Covariate-adjusted analysis

This file provides the **assumption and validation framework** for interpreting 
CT01 results.

---

## Key Takeaway

> Logistic regression results are only as reliable as the assumptions and model specification that support them.

---
