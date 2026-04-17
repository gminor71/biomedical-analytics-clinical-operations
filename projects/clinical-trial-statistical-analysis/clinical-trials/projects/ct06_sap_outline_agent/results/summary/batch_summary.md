# CT06 SAP Outline Agent Batch Summary

## Batch Overview

- **Total Cases:** 5
- **Successful Runs:** 3
- **Unsupported Runs:** 2

---

## Case Summaries

### sap_case_001

- **Study ID:** STUDY-001
- **Endpoint Type:** time_to_event
- **Endpoint Name:** overall_survival
- **Run Status:** success
- **Primary Method:** Kaplan-Meier + Log-rank + Cox PH

**Sensitivity Analyses**

- Assess proportional hazards assumption and consider RMST if the assumption is not supported.
- Review the impact of missing data assumptions on the primary analysis.

**Follow-Up Items**

- Confirm proportional hazards diagnostics for the primary regression approach.

**Warnings**

- PH assumption status is unknown or not assessed.
- Small sample size may reduce model stability and interpretability.
- Low event rate may reduce power and limit reliable inference.

**Notes**

- Intercurrent events were provided and should be aligned with the estimand strategy.
- Default TTE framework selected: KM estimation, Log-rank comparison, and Cox PH regression.
- Further diagnostics are needed before confirming the primary regression approach.
- Study design recorded as: randomized_controlled_trial.

---

### sap_case_002

- **Study ID:** STUDY-002
- **Endpoint Type:** time_to_event
- **Endpoint Name:** progression_free_survival
- **Run Status:** success
- **Primary Method:** Kaplan-Meier + Log-rank + Cox PH

**Sensitivity Analyses**

- Consider Stratified Cox model as an alternative time-to-event analysis approach.
- Consider Time-varying covariates as an alternative time-to-event analysis approach.
- Consider Restricted Mean Survival Time (RMST) as an alternative time-to-event analysis approach.
- Consider Accelerated Failure Time (AFT) model as an alternative time-to-event analysis approach.
- Consider sensitivity analysis addressing informative censoring.
- Review the impact of missing data assumptions on the primary analysis.

**Follow-Up Items**

- None

**Warnings**

- Proportional hazards assumption violated.
- Small sample size may reduce model stability and interpretability.
- Low event rate may reduce power and limit reliable inference.
- Potential informative censoring detected; sensitivity analysis should be considered.

**Notes**

- Intercurrent events were provided and should be aligned with the estimand strategy.
- Default TTE framework selected: KM estimation, Log-rank comparison, and Cox PH regression.
- Alternative methods recommended due to PH assumption violation.
- Study design recorded as: randomized_controlled_trial.

---

### sap_case_003

- **Study ID:** STUDY-003
- **Endpoint Type:** time_to_event
- **Endpoint Name:** disease_free_survival
- **Run Status:** success
- **Primary Method:** Kaplan-Meier + Log-rank + Cox PH

**Sensitivity Analyses**

- Assess proportional hazards assumption and consider RMST if the assumption is not supported.

**Follow-Up Items**

- Specify estimand strategy and handling of intercurrent events.
- Confirm proportional hazards diagnostics for the primary regression approach.
- Clarify relevant intercurrent events and how they will be handled in the estimand.

**Warnings**

- PH assumption status is unknown or not assessed.
- Small sample size may reduce model stability and interpretability.
- Low event rate may reduce power and limit reliable inference.

**Notes**

- No covariates were provided for the primary adjusted analysis.
- No stratification factors were provided.
- Default TTE framework selected: KM estimation, Log-rank comparison, and Cox PH regression.
- Further diagnostics are needed before confirming the primary regression approach.
- Study design recorded as: randomized_controlled_trial.

---

### sap_case_004

- **Study ID:** STUDY-004
- **Endpoint Type:** binary
- **Endpoint Name:** Unknown
- **Run Status:** unsupported
- **Primary Method:** Not assigned

**Sensitivity Analyses**

- None

**Follow-Up Items**

- None

**Warnings**

- Unsupported endpoint type: binary

**Notes**

- This v1 SAP outline agent currently supports only time-to-event logic.

---

### sap_case_004

- **Study ID:** STUDY-004
- **Endpoint Type:** binary
- **Endpoint Name:** Unknown
- **Run Status:** unsupported
- **Primary Method:** Not assigned

**Sensitivity Analyses**

- None

**Follow-Up Items**

- None

**Warnings**

- Unsupported endpoint type: binary

**Notes**

- This v1 SAP outline agent currently supports only time-to-event logic.

---
