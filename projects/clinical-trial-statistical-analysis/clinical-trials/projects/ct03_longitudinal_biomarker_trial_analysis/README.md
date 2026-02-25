# CT03 – Longitudinal Biomarker Trial Analysis

## Study Objective
Evaluate the effect of treatment on the longitudinal trajectory of a continuous biomarker.

## Primary Question
Does treatment with the Active intervention change the rate of biomarker decline
over time compared with Control?

## Design
- Randomized, two-arm clinical trial
- Intervention: Active vs Control
- Repeated biomarker measurements per subject
- Visit schedule: Day 0, 30, 90, 180, 365

## Primary Endpoint
Continuous biomarker measured repeatedly over time.

## Analysis Plan (summary)
- Linear mixed-effects model with random subject intercepts
- Fixed effects: treatment, time, and treatment × time interaction
- Covariate adjustment for age, sex, baseline severity, and BMI
- Primary estimand: difference in slope between treatment groups  
  (`trtActive:time30`)

## Secondary Question 1
- Is the treatment effect robust to alternative covariate adjustment?
- Sensitivity model excluding baseline severity
- Purpose: robustness

## Secondary Question 2
- Is there evidence of effect modification by baseline severity?
- Treatment × baseline severity × time interaction
- Purpose: exploratory

## Project Structure
- `src/` – Analysis scripts (00–07)
- `data/raw/` – Raw trial data (read-only)
- `data/processed/` – Analysis-ready longitudinal dataset
- `tables/` – Summary and model tables
- `figures/` – Trajectory plots and diagnostics
- `results/` – Model objects and metrics
- `rmd/` – Quarto reports (SAP, results)

## How to run
From the project root:
1. `source("src/99_run_all.R")`
