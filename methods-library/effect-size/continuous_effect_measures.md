# Continuous Effect Measures

## Overview

Continuous endpoints are commonly analyzed using methods that compare
central tendency or modeled mean differences between groups.

Effect measures for continuous data quantify differences in **magnitude**
rather than differences in event probability or time to event.

The most common measures include:

* Mean difference
* Least-squares mean difference
* Standardized mean difference

---

## Mean Difference

### Definition

The mean difference is the absolute difference in average outcome
between two groups.

### Interpretation

* Positive value → higher mean in treatment group
* Negative value → lower mean in treatment group
* Zero → no difference

### Use Case

* Standard effect measure for two-group comparisons
* Common output from t-tests and ANOVA-based contrasts

### Advantages

* Directly interpretable in the original measurement units
* Clinically meaningful when the scale is familiar

---

## Least-Squares Mean Difference

### Definition

The least-squares mean difference compares **model-adjusted group means**
estimated from a regression or ANCOVA framework.

### Interpretation

* Represents adjusted mean difference after accounting for covariates
* Useful when baseline imbalance or covariate adjustment is important

### Use Case

* Common in clinical trials using linear models or ANCOVA
* Frequently used when baseline values are included as covariates

### Advantages

* Adjusts for important prognostic variables
* Often preferred in confirmatory analyses with covariate adjustment

---

## Standardized Mean Difference

### Definition

The standardized mean difference expresses the difference between groups
relative to the pooled standard deviation.

### Interpretation

Common rough interpretation:

* 0.2 → small effect
* 0.5 → moderate effect
* 0.8 → large effect

### Use Case

* Comparing results across studies or scales
* Meta-analysis
* Situations where units differ across measures

### Limitations

* Less clinically intuitive than raw mean difference
* Depends on the variability of the sample

---

## Key Considerations

| Scenario | Preferred Measure |
|----------|------------------|
| Outcome scale is clinically meaningful | Mean difference |
| Covariate-adjusted analysis | Least-squares mean difference |
| Cross-study comparability needed | Standardized mean difference |

---

## Relationship to CT08

CT08 uses:

* **Mean difference** as the default effect measure for continuous endpoint analysis
* **Linear regression** as an adjusted method when covariates are present

In future extensions, adjusted models may explicitly report:

* **Least-squares mean difference**

---

## Interpretation Guidance

* Mean differences should be interpreted in the original clinical units whenever possible
* Adjusted mean differences are often more appropriate when baseline covariates influence outcome
* Standardized mean differences are helpful for comparison, but less useful for direct clinical interpretation

---

## Key Principle

> Effect measure selection for continuous endpoints should balance
statistical appropriateness, interpretability, and alignment with the analysis model.

---

## Future Enhancements

* Add ANCOVA-specific effect measure guidance
* Expand interpretation for repeated-measures models
* Link to continuous endpoint visualization guidance