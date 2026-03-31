# CT06 SAP Outline Agent Report

## Run Summary

- **Study ID:** STUDY-002
- **Analysis ID:** sap_case_002
- **Endpoint Type:** time_to_event
- **Endpoint Name:** progression_free_survival
- **Run Status:** success

---

## Draft SAP Outline

### Objective

Compare progression-free survival between treatment and control arms.

### Estimand

Primary estimand strategy: treatment_policy.

### Analysis Population

ITT

### Endpoint Definition

progression_free_survival will be analyzed as a time-to-event endpoint.

### Primary Method

Kaplan-Meier + Log-rank + Cox PH

### Covariate Strategy

Adjust for the following baseline covariates: age, baseline_tumor_burden.

### Stratification

Stratification factors: region, prior_therapy.

### Missing Data Handling

Missing data risk is assessed as high; censoring concern is informative.

---

## Sensitivity Analyses

- Consider Restricted Mean Survival Time (RMST) analysis.
- Consider Accelerated Failure Time (AFT) modeling.
- Consider time-varying covariate or stratified Cox approaches.
- Consider sensitivity analysis addressing informative censoring.
- Review the impact of missing data assumptions on the primary analysis.

---

## Follow-Up Items

- None

---

## Warnings

- Proportional hazards assumption violated.
- Potential informative censoring detected; sensitivity analysis should be considered.

---

## Notes

- Intercurrent events were provided and should be aligned with the estimand strategy.
- Study design recorded as: randomized_controlled_trial.

---
