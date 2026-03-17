# Time-to-Event Endpoint – Method Selection Guide

## Overview

Time-to-event (survival) endpoints are used in clinical trials to evaluate 
the time until an event occurs, such as:

* Death (Overall Survival, OS)
* Disease progression (Progression-Free Survival, PFS)
* Relapse or failure events

These endpoints require specialized methods that account for:

* Censoring
* Variable follow-up time
* Event timing

This guide provides a framework for selecting appropriate statistical 
methods based on study objectives and data characteristics.

---

## Step 1 – Define the Objective

### A. Describe Survival Experience

* Goal: Estimate survival probabilities over time

→ **Use: Kaplan-Meier Estimation**

---

### B. Compare Survival Between Groups

* Goal: Test if survival differs between treatment arms

→ **Use: Log-Rank Test**

---

### C. Estimate Treatment Effect (Adjust for Covariates)

* Goal: Quantify relative risk and adjust for confounders

→ **Use: Cox Proportional Hazards Model**

---

### D. Model Non-Proportional Effects or Time-Varying Risk

* Goal: Capture treatment effects that change over time

→ **Use:**

* Time-dependent Cox models
* Stratified Cox models

---

### E. Estimate Absolute Survival Differences

* Goal: Measure treatment benefit without PH assumption

→ **Use: Restricted Mean Survival Time (RMST)**

---

## Step 2 – Assess Key Data Characteristics

### 1. Censoring

* Is censoring present? (Almost always yes)

→ Use survival methods (KM, Cox, etc.)
→ Avoid standard regression methods

---

### 2. Proportional Hazards Assumption

* Is the treatment effect constant over time?

→ If YES:

* Cox PH model appropriate

→ If NO:

* RMST or time-dependent models preferred

---

### 3. Sample Size & Event Rate

* Low number of events reduces power

→ Consider:

* Simpler methods (KM + log-rank)
* Careful interpretation of hazard ratios

---

### 4. Covariate Adjustment Needed?

* Are baseline differences important?

→ If YES:

* Cox model (adjusted analysis)

---

### 5. Competing Risks Present?

* Multiple possible event types (e.g., death vs progression)

→ Consider:

* Competing risks methods (e.g., cumulative incidence functions)

---

## Method Summary

| Objective             | Method                    | Notes                        |
| --------------------- | ------------------------- | ---------------------------- |
| Estimate survival     | Kaplan-Meier              | Non-parametric               |
| Compare groups        | Log-rank test             | No covariate adjustment      |
| Adjust for covariates | Cox PH model              | Assumes proportional hazards |
| Non-PH scenarios      | RMST / time-dependent Cox | More flexible                |
| Competing risks       | CIF / Fine-Gray           | Specialized models           |

---

## Red Flags in Method Selection

* Using standard regression instead of survival methods
* Ignoring censoring
* Applying Cox model without assessing PH assumption
* Reporting hazard ratios when curves clearly cross
* Not adjusting for important covariates

---

## CRA & Clinical Operations Perspective

### What to Look For

* Does the selected method align with the endpoint definition in the SAP?
* Are censoring rules clearly defined and consistently applied?
* Is the Cox model supported by assumption checks?

---

### When to Escalate

* Method used does not match endpoint type
* Cox model used without evidence of PH assumption testing
* Discrepancy between Kaplan-Meier curves and reported hazard ratio
* Lack of clarity on censoring definitions

---

### Query Triggers

* Request justification for method selection
* Confirm inclusion of covariates in adjusted models
* Verify consistency with Statistical Analysis Plan (SAP)

---

## Relationship to Project Work

**CT02 – Time-to-Event Survival Analysis**

* Kaplan-Meier estimation → survival curves (F14-2-1)
* Log-rank test → group comparison
* Cox PH model → treatment effect estimation
* Schoenfeld residuals → PH assumption diagnostics (F14-6-1)

This guide defines **how methods are selected**, while 
CT02 demonstrates **how they are implemented**.

---

## Key Takeaway

> Method selection in survival analysis depends on the objective, assumptions, and data structure—not just the endpoint type.

---
