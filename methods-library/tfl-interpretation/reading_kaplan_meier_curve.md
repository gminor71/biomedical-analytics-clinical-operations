# How to Read a Kaplan-Meier Curve

## Overview

Kaplan-Meier (KM) curves are used in clinical trials to estimate and visualize 
time-to-event outcomes such as:

* Overall survival (OS)
* Progression-free survival (PFS)
* Time to event endpoints

They provide a **non-parametric estimate of survival probability over time** 
and are typically presented alongside:

* Log-rank test results
* Cox proportional hazards model outputs

---

## Key Components of a Kaplan-Meier Plot

### 1. Survival Curve

* Step function representing probability of survival over time
* Drops occur at observed event times

---

### 2. Censoring Marks

* Indicate subjects who did not experience the event during follow-up
* Typically shown as tick marks on the curve

---

### 3. X-Axis (Time)

* Time since baseline (e.g., randomization)
* Units: months, weeks, or days

---

### 4. Y-Axis (Survival Probability)

* Probability of remaining event-free
* Ranges from 0 to 1 (or 0% to 100%)

---

### 5. Number at Risk Table

* Displays number of subjects still at risk at different time points
* Critical for interpreting reliability of the curve over time

---

## How to Interpret a Kaplan-Meier Curve

### 1. Curve Separation

* **Early separation:** treatment effect occurs quickly
* **Late separation:** delayed treatment effect
* **No separation:** little or no difference between groups

---

### 2. Magnitude of Difference

* Vertical distance between curves reflects difference in survival probability
* Larger separation → greater potential treatment effect

---

### 3. Median Survival

* Time at which survival probability = 50%
* Compare medians between treatment groups

**Important:**
Median may not be reached if event rate is low

---

### 4. Consistency Over Time

* Stable separation suggests consistent treatment effect
* Converging curves may indicate diminishing effect

---

### 5. Reliability of Estimates

* As the number at risk decreases, estimates become less reliable
* Late time points with few subjects should be interpreted cautiously

---

## Statistical Context

### Log-Rank Test

* Tests for overall difference between survival curves
* Sensitive to differences across the entire follow-up period

---

### Hazard Ratio (Cox Model)

* Quantifies relative risk between groups
* Assumes proportional hazards over time

---

## Red Flags

* Crossing survival curves
* Large separation early that disappears later
* Very few subjects at risk in later time periods
* Heavy or imbalanced censoring between groups
* Median survival reported but curves are unstable

---

## Common Interpretation Pitfalls

### 1. Overreliance on Median Survival

* Does not capture full survival experience
* May miss early or late differences

---

### 2. Ignoring Curve Shape

* Two curves may have similar medians but very different patterns

---

### 3. Misinterpreting Censoring

* High censoring can distort interpretation of survival probability

---

### 4. Assuming Proportional Hazards

* KM curves may visually contradict Cox model assumptions

---

## CRA & Clinical Operations Perspective

### What to Look For

* Does the KM curve align with reported hazard ratio?
* Are censoring patterns balanced across treatment groups?
* Is the number at risk sufficient to support conclusions?

---

### When to Escalate

* KM curve contradicts reported treatment effect
* Crossing curves with no explanation
* Insufficient follow-up or high dropout rates
* Missing or unclear number at risk table

---

### Query Triggers

* Request clarification on discrepancies between KM and Cox results
* Confirm completeness of follow-up data
* Verify consistency with Statistical Analysis Plan (SAP)

---

## Relationship to Project Work

**CT02 – Time-to-Event Survival Analysis**

* Kaplan-Meier curves (F14-2-1)
* Cox Proportional Hazards model
* Schoenfeld residual diagnostics (F14-6-1)

This guide supports interpretation of survival outputs generated in CT02.

---

## Key Takeaway

> Kaplan-Meier curves provide the most direct view of survival experience—interpret the full curve, not just summary statistics.

---
