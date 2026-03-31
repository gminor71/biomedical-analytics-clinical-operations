# Biostatistics Glossary

A structured reference of key biostatistical concepts used across clinical 
research, epidemiology, and data analysis.

---

## Study Design

**Randomization**
Assignment of participants to treatment groups by chance.
**Why it matters:** Reduces selection bias and confounding.

**Blinding**
Concealing treatment assignment from participants and/or investigators.
**Types:** Single-blind, double-blind, triple-blind.

**Confounding**
A distortion of the relationship between exposure and outcome due to a 
third variable.

**Bias**
Systematic error that leads to incorrect estimates of effect.

---

## Data Types

**Continuous**
Numeric data measured on a continuous scale.
**Examples:** Blood pressure, weight.

**Categorical**
Data divided into groups or categories.
**Types:** Binary, nominal, ordinal.

**Time-to-Event (Survival Data)**
Time until an event occurs (e.g., death, relapse).
**Feature:** May include censored observations.

---

## Descriptive Statistics

**Mean**
Arithmetic average of values.
**Limitation:** Sensitive to outliers.

**Median**
Middle value of ordered data.
**Use when:** Data is skewed.

**Standard Deviation (SD)**
Measure of variability around the mean.

**Interquartile Range (IQR)**
Range between 25th and 75th percentiles.
**Use when:** Data is skewed.

---

## Inferential Statistics

**Hypothesis Testing**
Framework for testing assumptions about a population.

**p-value**
Probability of observing data under the null hypothesis.
**Interpretation:** Smaller values suggest evidence against H₀.

**Confidence Interval (CI)**
Range of plausible values for a parameter estimate.

**Statistical Significance**
Determined when p-value < predefined threshold (e.g., 0.05).

---

## Modeling

**Linear Regression**
Models relationship between continuous outcome and predictors.

**Logistic Regression**
Models probability of a binary outcome.
**Output:** Odds ratios.

**Cox Proportional Hazards Model**
Analyzes time-to-event data using hazard ratios.

---

## Clinical Trial Terms

**Intention-to-Treat (ITT)**
Includes all randomized participants in analysis regardless of adherence.

**Per Protocol (PP)**
Includes only participants who completed the study per protocol.

**Endpoint**
Primary outcome used to assess treatment effect.

**Adverse Event (AE)**
Any unfavorable medical occurrence during a study.

---

## Data Quality & Monitoring

**Missing Data**
Absence of values in dataset.
**Types:** MCAR, MAR, MNAR.

**Outlier**
Extreme observation differing from other values.

**Data Query**
Request for clarification or correction of data discrepancies.

**Protocol Deviation**
Departure from approved study protocol.

---


## Cross-References

See:

* biostat_methods_reference.md (method selection and execution)
