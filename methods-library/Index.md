# Methods Library – Index

## Overview

This index provides a structured guide to statistical methods, 
decision frameworks, and interpretation resources used in clinical trial analysis.

Use this page as the primary entry point to navigate the Methods Library.

---

## Project Foundation

### Biostatistics Project Template

* [Biostatistics Project Template](./biostatistics-project-template/README.md)

Provides the standardized structure for building reproducible and auditable 
clinical trial analyses.

---

## Statistical Decision Guides

Frameworks for selecting appropriate statistical methods based on endpoint 
type and study design.

### Binary Endpoints

* [Binary Endpoint – Method Selection Guide](./statistical-decision-guides/binary_endpoint_method_selection.md)

### Time-to-Event Endpoints

* [Time-to-Event – Method Selection Guide](./statistical-decision-guides/time_to_event_method_selection.md)

---

## Model Assumptions

Validation checklists and diagnostic approaches for statistical models.

### Binary Outcomes

* [Logistic Regression – Assumptions & Diagnostics](./model-assumptions/logistic_regression_assumptions.md)

### Time-to-Event Outcomes

* [Cox Proportional Hazards – Assumptions & Diagnostics](./model-assumptions/cox_ph_assumptions.md)

---

## TFL Interpretation

Guides for interpreting Tables, Listings, and Figures (TFLs).

### Binary Outcomes

* [How to Read Logistic Regression Output](./tfl-interpretation/reading_logistic_regression_output.md)

### Time-to-Event Outcomes

* [How to Read a Kaplan-Meier Curve](./tfl-interpretation/reading_kaplan_meier_curve.md)

---

## Missing Data

Handling missing data in clinical trials.

* Missing Data Methods & Considerations

---

## Effect Size & Significance

Understanding statistical vs clinical importance.

* *(Coming Soon)* Effect Size vs Statistical Significance

---

## Common Pitfalls

Frequent issues in clinical trial statistical analysis.

* Common Statistical Pitfalls in Clinical Trials

---

## Agent Logic

Structured decision frameworks that translate statistical methods and 
clinical workflows into actionable logic.

This section extends the Methods Library from:

Method guidance → to **Decision-making systems and automation-ready workflows**

**Includes:**

* Clinical operations logic (e.g., CRA query triage, site risk scoring)
* Biostatistical logic (e.g., method selection, model diagnostics)
* Cross-functional decision frameworks

[Agent Logic Index](./agent-logic/INDEX.md)

---
## Relationship to Project Work

### CT01 – Binary Endpoint Trial Analysis

**Methods Covered:**

* Binary Endpoint Method Selection
* Logistic Regression Assumptions
* Logistic Regression Output Interpretation

---

### CT02 – Time-to-Event Survival Analysis

**Methods Covered:**

* Time-to-Event Method Selection
* Cox Proportional Hazards Assumptions
* Kaplan-Meier Interpretation

---

## Recommended Workflow

1. **Select Method**

   * Use Statistical Decision Guides

2. **Validate Model**

   * Apply Model Assumptions & Diagnostics

3. **Interpret Results**

   * Use TFL Interpretation Guides

4. **Translate to Decision Logic (NEW)**

   * Use Agent Logic to convert methods into structured decision frameworks
   
5. **Apply to Project Work**

   * Reference CT01 / CT02 implementations

---

## Key Principle

> Method selection, validation, and interpretation should function as 
a connected system—not isolated steps.

---
