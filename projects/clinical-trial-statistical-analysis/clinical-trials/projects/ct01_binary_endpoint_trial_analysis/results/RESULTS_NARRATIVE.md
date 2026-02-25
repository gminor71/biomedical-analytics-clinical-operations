# CT02 - TIME-TO-EVENT TRIAL Results Narrative

Generated: 2026-02-25 09:54:41

## Executive Summary
- Primary estimand: Adjusted HR (Active vs Control): 0.67 (95% CI 0.55–0.83); p=<0.001.
- Sensitivity and secondary analyses were reviewed for consistency with the primary conclusion.
- Diagnostics: The proportional hazards assumption was assessed using Schoenfeld residuals (global p=0.630).

## Analysis Set and Data Overview
The primary analysis was conducted in the intention-to-treat (ITT) population (all randomized subjects analyzed by assigned treatment).
Key QC outputs were used as the source of truth for dataset integrity and analysis reproducibility (see Appendix).

## Baseline Characteristics (Table 1)
Baseline characteristics were summarized descriptively by treatment group (Table 1). No hypothesis testing was performed for baseline comparisons.

## Primary Efficacy Results
In the primary adjusted Cox proportional hazards model (adjusted for age, sex, and baseline severity), Active treatment was associated with a hazard ratio of 0.67 (95% CI 0.55–0.83) compared with Control (p=<0.001).

## Secondary and Sensitivity Analyses
Secondary and sensitivity analyses (as prespecified) were generated to evaluate robustness of the primary findings and explore effect modification.

## Model Diagnostics and Assumptions
The proportional hazards assumption was assessed using Schoenfeld residuals (global p=0.630).

## Conclusion
Overall, the primary model and prespecified supportive analyses provide the basis for the study interpretation. Results should be interpreted in the context of model assumptions and the exploratory nature of interaction assessments.

## Appendix: Output Traceability (No Re-run)
This narrative was created by reading the following frozen outputs (not by refitting models):
- **Primary model (RDS)**: `C:/Users/grant/OneDrive/Documents/GitHub/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/primary_model.rds` (OK)
- **Secondary models (RDS)**: `C:/Users/grant/OneDrive/Documents/GitHub/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/secondary_models.rds` (OK)
- **Table 1 object (RDS)**: `C:/Users/grant/OneDrive/Documents/GitHub/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/table1_baseline.rds` (OK)
- **EMMEANS (CSV)**: `C:/Users/grant/OneDrive/Documents/GitHub/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/tables/primary_emmeans_by_visit.csv` (MISSING)
- **PH test (CSV)**: `C:/Users/grant/OneDrive/Documents/GitHub/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/tables/diagnostics_ph_test.csv` (OK)
- **CT01 interaction OR (CSV)**: `C:/Users/grant/OneDrive/Documents/GitHub/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/tables/interaction_or_by_severity.csv` (OK)
- **CT02 interaction HR (CSV)**: `C:/Users/grant/OneDrive/Documents/GitHub/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/tables/interaction_hr_by_severity.csv` (OK)

QC artifacts referenced:
- `C:/Users/grant/OneDrive/Documents/GitHub/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/QC_02_data_cleaning.txt`
- `C:/Users/grant/OneDrive/Documents/GitHub/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/QC_03_table1.txt` (if present)
- `C:/Users/grant/OneDrive/Documents/GitHub/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/QC_04_primary_analysis.txt`
- `C:/Users/grant/OneDrive/Documents/GitHub/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct01_binary_endpoint_trial_analysis/results/QC_05_secondary_analyses.txt`
