# CT02 – Time-to-Event (Survival) Trial Analysis

## Study Objective
Evaluate the effect of treatment on time to clinical event.

## Primary Question
Does treatment with the Active intervention prolong the time to
the clinical event compared with Control?

## Design
- Randomized, two-arm clinical trial
- Intervention: Active vs Control
- Follow-up: Up to 365 days
- Subjects may be censored due to loss to follow-up or administrative censoring

## Primary Endpoint
Time from randomization to event or censoring.

## Analysis Plan (summary)
- Cox proportional hazards model
- Treatment as primary predictor
- Covariate adjustment for age, sex, and baseline disease severity
- Primary estimand: Hazard Ratio (Active vs Control) with 95% CI

## Secondary Question 1
- Is the treatment effect robust to alternative covariate adjustment?
- Sensitivity model excluding baseline severity
- Purpose: robustness

## Secondary Question 2
- Is there evidence of effect modification by baseline severity?
- Treatment × baseline severity interaction
- Purpose: exploratory

## Project Structure
- `src/` – Analysis scripts (00–07)
- `data/raw/` – Raw trial data (read-only)
- `data/processed/` – Analysis-ready survival dataset
- `tables/` – Summary and model tables
- `figures/` – Kaplan–Meier curves and diagnostics
- `results/` – Model objects and metrics
- `rmd/` – Quarto reports (SAP, results)

## How to run
From the project root:
1. `source("src/99_run_all.R")`
