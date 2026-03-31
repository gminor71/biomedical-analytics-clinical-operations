# Binary Effect Measures

## Overview

Binary endpoints are commonly analyzed using:

* Risk difference
* Risk ratio
* Odds ratio

---

## Risk Difference (RD)

### Definition

Difference in event probabilities between groups.

### Interpretation

* Absolute effect
* Clinically intuitive

### Use Case

* Preferred for clinical interpretation
* Regulatory relevance

---

## Risk Ratio (RR)

### Definition

Ratio of event probabilities between groups.

### Interpretation

* Relative effect
* Easy to communicate

### Use Case

* Common in epidemiology
* Cohort studies

---

## Odds Ratio (OR)

### Definition

Ratio of odds between groups.

### Interpretation

* Less intuitive than RR
* Can exaggerate effect when events are common

### Use Case

* Logistic regression output
* Case-control studies

---

## Key Considerations

| Scenario | Preferred Measure |
|--------|------------------|
| Clinical interpretation | Risk difference |
| Relative comparison | Risk ratio |
| Adjusted analysis | Odds ratio |

---

## Relationship to CT07

CT07 uses:

* RD / RR for unadjusted comparisons
* OR when logistic regression is applied