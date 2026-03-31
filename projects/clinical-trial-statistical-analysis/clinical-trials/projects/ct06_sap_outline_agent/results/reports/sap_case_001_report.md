# CT06 SAP Outline Agent Report

## Run Summary

- **Study ID:** STUDY-001
- **Analysis ID:** sap_case_001
- **Endpoint Type:** time_to_event
- **Endpoint Name:** overall_survival
- **Run Status:** success

---

## Draft SAP Outline

### Objective

Compare overall survival between treatment and control arms.

### Estimand

Primary estimand strategy: treatment_policy.

### Analysis Population

ITT

### Endpoint Definition

overall_survival will be analyzed as a time-to-event endpoint.

### Primary Method

Kaplan-Meier + Log-rank + Cox PH

### Covariate Strategy

Adjust for the following baseline covariates: age, sex, baseline_ecog.

### Stratification

Stratification factors: region, baseline_disease_stage.

### Missing Data Handling

Missing data risk is assessed as moderate; censoring concern is non_informative.

---

## Sensitivity Analyses

- Assess proportional hazards assumption and consider RMST if the assumption is not supported.
- Review the impact of missing data assumptions on the primary analysis.

---

## Follow-Up Items

- Confirm proportional hazards diagnostics for the primary regression approach.

---

## Warnings

- None

---

## Notes

- Intercurrent events were provided and should be aligned with the estimand strategy.
- Study design recorded as: randomized_controlled_trial.

---
