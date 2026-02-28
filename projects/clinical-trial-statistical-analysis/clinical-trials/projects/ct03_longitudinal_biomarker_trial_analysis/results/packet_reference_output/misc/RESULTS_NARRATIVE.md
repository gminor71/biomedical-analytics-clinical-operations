# CT03 - Longitudinal Biomarker Trial Results Narrative

Generated: 2026-02-27 16:19:03

## Executive Summary
- Primary estimand: Treatment-by-time interaction (per 30 days): beta=-1.7710 (SE=0.0676); p=<0.001.
- Sensitivity and secondary analyses were reviewed for consistency with the primary conclusion.
- Diagnostics: Model convergence and fit diagnostics were reviewed; estimation used maximum likelihood under a MAR assumption.

## Analysis Set and Data Overview
The primary analysis was conducted in the intention-to-treat (ITT) population (all randomized subjects analyzed by assigned treatment).
Key QC outputs were used as the source of truth for dataset integrity and analysis reproducibility (see Appendix).

## Baseline Characteristics (Table 1)
Baseline characteristics were summarized descriptively by treatment group (Table 1). No hypothesis testing was performed for baseline comparisons.

## Primary Efficacy Results
In the primary linear mixed-effects model, the prespecified treatment-by-time interaction (difference in slope per 30 days) was -1.7710 (SE=0.0676; p=<0.001).

## Secondary and Sensitivity Analyses
Secondary and sensitivity analyses (as prespecified) were generated to evaluate robustness of the primary findings and explore effect modification.

## Model Diagnostics and Assumptions
Model convergence and fit diagnostics were reviewed; estimation used maximum likelihood under a MAR assumption.
Estimated marginal means were produced at prespecified visits (0, 30, 90, 180, 365 days).

## Conclusion
Overall, the primary model and prespecified supportive analyses provide the basis for the study interpretation. Results should be interpreted in the context of model assumptions and the exploratory nature of interaction assessments.

## Appendix: Output Traceability (No Re-run)
This narrative was created by reading the following frozen outputs (not by refitting models):
- **Primary model (RDS)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct03_longitudinal_biomarker_trial_analysis/results/primary_model.rds` (OK)
- **Secondary models (RDS)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct03_longitudinal_biomarker_trial_analysis/results/secondary_models.rds` (OK)
- **Table 1 object (RDS)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct03_longitudinal_biomarker_trial_analysis/results/table1_baseline.rds` (OK)
- **EMMEANS (CSV)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct03_longitudinal_biomarker_trial_analysis/tables/primary_emmeans_by_visit.csv` (OK)
- **PH test (CSV)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct03_longitudinal_biomarker_trial_analysis/tables/diagnostics_ph_test.csv` (MISSING)
- **CT01 interaction OR (CSV)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct03_longitudinal_biomarker_trial_analysis/tables/interaction_or_by_severity.csv` (MISSING)
- **CT02 interaction HR (CSV)**: `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct03_longitudinal_biomarker_trial_analysis/tables/interaction_hr_by_severity.csv` (MISSING)

QC artifacts referenced:
- `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct03_longitudinal_biomarker_trial_analysis/results/QC_02_data_cleaning.txt`
- `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct03_longitudinal_biomarker_trial_analysis/results/QC_03_table1.txt` (if present)
- `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct03_longitudinal_biomarker_trial_analysis/results/QC_04_primary_analysis.txt`
- `C:/Dev/biomedical-analytics-clinical-operations/projects/clinical-trial-statistical-analysis/clinical-trials/projects/ct03_longitudinal_biomarker_trial_analysis/results/QC_05_secondary_analyses.txt`
