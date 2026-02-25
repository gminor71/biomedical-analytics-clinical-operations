# CT04 – Dose Finding and Pharmacokinetic Trial Analysis

## Study Objective
Evaluate the relationship between dose level, pharmacokinetic exposure, and clinical response in a dose-escalation clinical trial.

## Primary Question
Is increasing dose associated with increased pharmacokinetic exposure and improved clinical response while maintaining acceptable safety?

## Design
- Randomized, multi-dose clinical trial
- Dose levels: Low, Medium, High
- Repeated pharmacokinetic sampling per subject
- Clinical response assessed at predefined visits
- Safety monitoring across dose cohorts
- Early-phase dose-finding design intended to characterize exposure, safety, and preliminary efficacy

## Primary Endpoint
Pharmacokinetic exposure summarized using derived parameters (e.g., AUC, Cmax).

## Analysis Plan (summary)
- Descriptive summary of PK parameters by dose group
- Linear regression of exposure on dose level
- Dose–response modeling of clinical outcome

## Primary estimand: change in response associated with increasing exposure

## Secondary Question 1
- Is clinical response more strongly associated with exposure than nominal dose?
- Exposure–response regression using continuous exposure metrics
- Purpose: characterize exposure-response relationship

## Secondary Question 2
- Is pharmacokinetic exposure approximately proportional to dose?
- Log-linear regression of exposure metrics (AUC, Cmax) on dose level
- Purpose: evaluate dose proportionality across studied dose range

## Secondary Question 3
- Are safety events associated with higher dose or exposure levels?
- Summary of adverse events by dose group
- Purpose: safety characterization

## Project Structure
- src/ – Analysis scripts (00–07)
- data/raw/ – Raw trial data (read-only)
- data/processed/ – Analysis-ready PK and endpoint datasets
- tables/ – PK summaries and model tables
- figures/ – Dose-response and exposure-response plots
- results/ – Model objects and output packets
- rmd/ – Quarto reports (SAP, TFL shells, results)

## How to run
From the project root:
1. source("src/99_run_all.R")