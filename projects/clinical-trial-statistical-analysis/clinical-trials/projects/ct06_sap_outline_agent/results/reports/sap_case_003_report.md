# CT06 SAP Outline Agent Report

## Run Summary

- **Study ID:** STUDY-003
- **Analysis ID:** sap_case_003
- **Endpoint Type:** time_to_event
- **Endpoint Name:** disease_free_survival
- **Run Status:** success

---

## Draft SAP Outline

### Objective

Evaluate disease-free survival across treatment groups.

### Estimand

Estimand strategy not fully specified.

### Analysis Population

PER_PROTOCOL

### Endpoint Definition

disease_free_survival will be analyzed as a time-to-event endpoint.

### Primary Method

Kaplan-Meier + Log-rank + Cox PH

### Covariate Strategy

No baseline covariates specified.

### Stratification

No stratification factors specified.

### Missing Data Handling

Missing data risk is assessed as low; censoring concern is non_informative.

---

## Sensitivity Analyses

- Assess proportional hazards assumption and consider RMST if the assumption is not supported.

---

## Follow-Up Items

- Specify estimand strategy and handling of intercurrent events.
- Confirm proportional hazards diagnostics for the primary regression approach.
- Clarify relevant intercurrent events and how they will be handled in the estimand.

---

## Warnings

- None

---

## Notes

- No covariates were provided for the primary adjusted analysis.
- No stratification factors were provided.
- Study design recorded as: randomized_controlled_trial.

---
