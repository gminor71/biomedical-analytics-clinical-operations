# Time-to-Event Effect Measures

## Overview

Time-to-event (TTE) endpoints are commonly analyzed using survival analysis methods.
Effect measures for TTE data quantify differences in **time until an event occurs**
between treatment groups.

The most common measures include:

* Hazard ratio (HR)
* Median survival difference
* Survival probability at fixed timepoints
* Restricted Mean Survival Time (RMST)

---

## Hazard Ratio (HR)

### Definition

The hazard ratio compares the **instantaneous event rate** between two groups
over time.

### Interpretation

* HR < 1 → reduced hazard in treatment group
* HR > 1 → increased hazard in treatment group
* HR = 1 → no difference

### Key Properties

* Derived from Cox proportional hazards model
* Assumes proportional hazards over time

### Use Case

* Standard primary effect measure in many clinical trials
* Regulatory familiarity and acceptance

---

## Median Survival Difference

### Definition

Difference in median time-to-event between groups.

### Interpretation

* Represents the time at which 50% of subjects have experienced the event

### Limitations

* May not be estimable if median is not reached
* Does not use full survival curve information

---

## Survival Probability at Fixed Timepoints

### Definition

Comparison of survival probabilities at specific timepoints (e.g., 6 months, 1 year).

### Interpretation

* Direct and clinically meaningful
* Answers: “What proportion survive beyond time X?”

### Use Case

* Supplementary analysis
* Clinical communication

---

## Restricted Mean Survival Time (RMST)

### Definition

Average survival time up to a specified time horizon.

### Interpretation

* Difference in expected survival time between groups
* Does not rely on proportional hazards assumption

### Advantages

* Robust when PH assumption is violated
* Clinically interpretable as “time gained”

### Use Case

* Alternative to hazard ratio when PH assumption is violated
* Increasing regulatory interest

---

## Key Considerations

| Scenario | Preferred Measure |
|----------|------------------|
| PH assumption holds | Hazard ratio |
| PH assumption violated | RMST |
| Clinical interpretability needed | RMST or survival probability |
| Median not reached | Avoid median survival |

---

## Relationship to CT05

CT05 uses:

* **Hazard ratio** as the primary effect measure via Cox PH
* **RMST** as a key alternative when PH assumption is violated

---

## Interpretation Guidance

* Hazard ratios describe **relative risk over time**, not absolute time gained
* RMST provides **absolute time-based interpretation**
* Survival curves should always accompany effect measures for context

---

## Key Principle

> Effect measure selection for time-to-event endpoints should balance
statistical validity (assumptions) and clinical interpretability.

---

## Future Enhancements

* Integration with automated PH diagnostics
* Visualization guidance for survival curves
* Extension to competing risks and multi-state models