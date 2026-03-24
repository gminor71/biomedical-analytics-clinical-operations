# CT01 - BINARY ENDPOINT TRIAL Results Narrative

Generated: 2026-03-24 16:43:38

## Trial Design Evaluation
A simulation-based design evaluation was conducted using frozen design outputs generated before formal results reporting.

### Design Assumptions
- Control event rate: 0.35
- Treatment event rate: 0.55
- Absolute treatment effect: 0.20
- Significance level (alpha): 0.05
- Target power: 0.80
- Planned sample size: 96 subjects per arm
- Number of simulations: 5000

### Design Results
- Analytical sample size required: 100 subjects per arm
- Mean simulated control proportion: 0.3492
- Mean simulated treatment proportion: 0.5507
- Empirical power: 0.8070

### Operating Characteristics
- Mean type I error across null scenarios: 0.0491
- Mean confidence interval coverage: 0.9460
- Power at the assumed design effect: 0.8076

### Design Interpretation
Analytical and simulation-based results support the planned sample size of 96 subjects per arm for detecting an absolute treatment effect of 0.20.

## Executive Summary
- Primary estimand: Adjusted OR (Active vs Control): 0.58 (95% CI 0.32–1.03); p=0.065.
- Sensitivity and secondary analyses were reviewed for consistency with the primary conclusion.
- Diagnostics: The primary model converged and standard model checks were reviewed (influence and collinearity outputs).

## Analysis Set and Data Overview
The primary analysis was conducted in the intention-to-treat (ITT) population (all randomized subjects analyzed by assigned treatment).
Key QC outputs were used as the source of truth for dataset integrity and analysis reproducibility (see Appendix).

## Baseline Characteristics (Table 1)
Baseline characteristics were summarized descriptively by treatment group (Table 1). No hypothesis testing was performed for baseline comparisons.

## Primary Efficacy Results
In the primary adjusted logistic regression model (adjusted for age, sex, and baseline severity), Active treatment was associated with an adjusted odds ratio of 0.58 (95% CI 0.32–1.03) compared with Control (p=0.065).

## Secondary and Sensitivity Analyses
Secondary and sensitivity analyses (as prespecified) were generated to evaluate robustness of the primary findings and explore effect modification.

## Model Diagnostics and Assumptions
The primary model converged and standard model checks were reviewed (influence and collinearity outputs).

## Conclusion
Overall, the primary model and prespecified supportive analyses provide the basis for the study interpretation. Results should be interpreted in the context of model assumptions and the exploratory nature of interaction assessments.

## Appendix: Output Traceability (No Re-run)
This narrative was created by reading the following frozen outputs (not by refitting models):
- **Primary model (RDS)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/primary_model.rds` (OK)
- **Secondary models (RDS)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/secondary_models.rds` (OK)
- **Table 1 object (RDS)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/table1_baseline.rds` (OK)
- **EMMEANS (CSV)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/tables/primary_emmeans_by_visit.csv` (MISSING)
- **PH test (CSV)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/tables/diagnostics_ph_test.csv` (OK)
- **CT01 interaction OR (CSV)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/tables/interaction_or_by_severity.csv` (OK)
- **CT02 interaction HR (CSV)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/tables/interaction_hr_by_severity.csv` (OK)
- **Design simulation summary (CSV)**: `design/results/simulation_summary.csv` (OK)
- **Design operating characteristics**: `design/results/operating_characteristics.csv` (OK)
- **Design power curve (CSV)**: `design/results/power_curve.csv` (OK)
- **Design power figure (PNG)**: `design/results/power_curve.png` (OK)

QC artifacts referenced:
- `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/QC_02_data_cleaning.txt`
- `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/QC_03_table1.txt` (if present)
- `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/QC_04_primary_analysis.txt`
- `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/QC_05_secondary_analyses.txt`
