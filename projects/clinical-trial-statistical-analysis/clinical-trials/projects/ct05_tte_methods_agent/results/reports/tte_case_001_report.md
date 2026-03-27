# CT05 TTE Method Agent Report

## Run Summary

- **Study ID:** CT05
- **Analysis ID:** tte_case_001
- **Endpoint Type:** time_to_event
- **Endpoint Name:** progression_free_survival
- **Run Status:** success

---

## Primary Recommendation

- **Recommended Method:** Kaplan-Meier + Log-rank + Cox PH
- **PH Assumption Status:** violated

---

## Alternative Methods

- Stratified Cox model
- Time-varying covariates
- Restricted Mean Survival Time (RMST)
- Accelerated Failure Time (AFT) model

---

## Warnings

- Proportional hazards assumption violated.

---

## Notes

- Default TTE framework selected: KM estimation, Log-rank comparison, and Cox PH regression.
- Alternative methods recommended due to PH assumption violation.

---
