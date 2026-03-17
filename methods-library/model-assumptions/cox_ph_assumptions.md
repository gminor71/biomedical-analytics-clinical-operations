# Cox Proportional Hazards Model – Assumptions & Diagnostics

## Overview

The Cox Proportional Hazards (PH) model is commonly used in clinical trials to 
evaluate time-to-event outcomes.

The validity of model results depends on key assumptions. Violations of these
assumptions can lead to:

* Biased hazard ratio estimates
* Misleading treatment effects
* Incorrect clinical or regulatory conclusions

This guide outlines the core assumptions, how to assess them, and actions to 
take if violations are detected.

---

## Core Assumptions

### 1. Proportional Hazards (PH Assumption)

The hazard ratio between groups is constant over time.

**Interpretation:**
The treatment effect does not change throughout the study period.

---

### 2. Independence of Survival Times

Event times are assumed to be independent across subjects.

**Risk Factors:**

* Clustered data (e.g., sites, centers)
* Repeated measures without adjustment

---

### 3. Correct Covariate Specification

Covariates are correctly modeled (e.g., linearity, no omitted variables).

**Examples:**

* Continuous variables may require transformation
* Interaction terms may be necessary

---

### 4. Non-informative Censoring

Censoring is unrelated to the probability of the event.

**Risk Factors:**

* Dropout due to disease progression
* Differential loss to follow-up between treatment arms

---

## Diagnostics & Evaluation

### 1. Schoenfeld Residuals

Used to test the proportional hazards assumption.

**Methods:**

* Statistical test for time-dependence
* Plot residuals vs time

**Interpretation:**

* No trend → PH assumption likely satisfied
* Trend over time → potential violation

---

### 2. Log(-Log(Survival)) Plots

Graphical method to assess proportional hazards.

**Interpretation:**

* Parallel curves → PH assumption reasonable
* Crossing or diverging curves → violation likely

---

### 3. Kaplan-Meier Curve Review

Visual inspection of survival curves.

**Red Flags:**

* Crossing survival curves
* Early vs late separation patterns

---

### 4. Time-Dependent Covariate Testing

Include interaction terms with time.

**Example:**

* Treatment × time interaction

**Interpretation:**

* Significant interaction → PH assumption violated

---

## Red Flags

* Crossing Kaplan-Meier curves
* Non-parallel log(-log) survival plots
* Significant Schoenfeld residual test
* Treatment effect changes over time
* High or uneven censoring between groups

---

## Actions if Assumptions Are Violated

### 1. Stratified Cox Model

* Stratify by variables violating PH
* Removes need to estimate their effect directly

---

### 2. Time-Dependent Covariates

* Model changing effects over time
* Example: treatment effect varies by follow-up duration

---

### 3. Alternative Models

* Accelerated Failure Time (AFT) models
* Restricted Mean Survival Time (RMST)

---

### 4. Sensitivity Analyses

* Compare results across multiple model specifications
* Assess robustness of conclusions

---

## CRA & Clinical Operations Perspective

### What to Look For

* Are diagnostics (e.g., Schoenfeld residuals) included in outputs?
* Do Kaplan-Meier curves visually support model assumptions?
* Is the reported hazard ratio consistent with observed survival patterns?

---

### When to Escalate

* PH assumption clearly violated with no adjustment
* Inconsistency between Kaplan-Meier curves and hazard ratio
* Lack of documented diagnostics in analysis outputs
* Unexplained changes in treatment effect over time

---

### Query Triggers

* Request confirmation of PH assumption testing
* Request sensitivity analyses if violations suspected
* Verify alignment with Statistical Analysis Plan (SAP)

---

## Relationship to Project Work

**CT02 – Time-to-Event Survival Analysis**

* Kaplan-Meier estimation
* Cox Proportional Hazards model
* Schoenfeld residual diagnostics (F14-6-1)

This file provides the **theoretical and diagnostic foundation** for 
interpreting CT02 outputs.

---

## Key Takeaway

> The Cox model does not inherently guarantee valid results—its assumptions must be verified and supported by diagnostics.

---
