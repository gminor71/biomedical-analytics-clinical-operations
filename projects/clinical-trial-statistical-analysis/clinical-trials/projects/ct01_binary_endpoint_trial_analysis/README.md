# CT01 – Binary Endpoint Trial Analysis

## Study Objective
Evaluate the effect of treatment on a binary clinical endpoint.

## Primary Question
Does treatment with the Active intervention reduce the risk of experiencing 
the clinical event within 365 days compared with Control?

## Design
- Randomized, two-arm clinical trial
- Intervention: Active vs Control
- Follow-up: Up to 365 Days

Project-specific trial design work is located in `design/`.

The design module evaluates a Phase II two-arm randomized trial with a binary
endpoint using:
- analytical power analysis
- simulation under assumed response rates
- operating characteristic assessment

Design outputs are written to `design/results/`.
Design validation (power, simulation, operating characteristics) is 
integrated into the reporting packet.

## Primary Endpoint
Binary event occurrence during follow-up.

## Analysis Plan (summary)
- Logistic regression with treatment as primary predictor
- Covariate adjustment for age, sex, and baseline disease severity
- Adjusted Odds Ratio with 95% CI

## Secondary Question 1
- Is the treatment effect consistent after alternative covariate adjustment?
- Model with and without baseline severity
- Purpose: robustness

## Secondary Question 2
- Is there evidence of effect modification by baseline severity?
- Treatment x baseline severity interaction
- Purpose: exploratory

## Project Structure
- `src/` – Analysis scripts (00–07)
- `data/raw/` – Raw trial data (read-only)
- `data/processed/` – Analysis-ready datasets
- `tables/` – Summary and model tables
- `figures/` – Plots and diagnostics
- `results/` – Model objects and metrics
- `rmd/` – Quarto reports (SAP, results)

## How to run
From the project root:
1. `source("src/00_setup.R")`
2. `source("src/01_data_import.R")`
3. Run remaining scripts in numeric order

## Related Methods

- Logistic Regression Assumptions  
  → ../../methods-library/model-assumptions/logistic_regression_assumptions.md

- How to Read Logistic Output  
  → ../../methods-library/tfl-interpretation/how_to_read_logistic_output.md

- Binary Endpoint Method Selection  
  → ../../methods-library/statistical-decision-guides/binary_endpoint_method_selection.md