# CT05 TTE Method Agent – Batch Summary

## Overview

- Total cases processed: 2

---

## Case Summary

| Analysis ID | Endpoint Name | Run Status | PH Status | Primary Method | Warnings |
|---|---|---|---|---|---:|
| tte_case_001 | progression_free_survival | success | violated | Kaplan-Meier + Log-rank + Cox PH | 1 |
| tte_case_002 | overall_survival | success | unknown | Kaplan-Meier + Log-rank + Cox PH | 4 |

---

## Warning Details

### tte_case_001
- Proportional hazards assumption violated.

### tte_case_002
- PH assumption status is unknown or not assessed.
- Small sample size may reduce model stability and interpretability.
- Low event rate may reduce power and limit reliable inference.
- Potential informative censoring detected; sensitivity analysis should be considered.
