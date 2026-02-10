# CT03 - Longitudinal Biomarker Trial Results Narrative

Generated: 2026-02-06 16:19:16

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
- Primary model (RDS): `C:/Users/grant/OneDrive/Documents/GitHub/bio-epi-portfolio/projects/clinical-trials/projects/ct03_longitudinal/results/primary_model.rds`
- Secondary models (RDS): `C:/Users/grant/OneDrive/Documents/GitHub/bio-epi-portfolio/projects/clinical-trials/projects/ct03_longitudinal/results/secondary_models.rds`
- Table 1 object (RDS): `C:/Users/grant/OneDrive/Documents/GitHub/bio-epi-portfolio/projects/clinical-trials/projects/ct03_longitudinal/results/table1_baseline.rds`
- EMMEANS (CSV): `C:/Users/grant/OneDrive/Documents/GitHub/bio-epi-portfolio/projects/clinical-trials/projects/ct03_longitudinal/tables/primary_emmeans_by_visit.csv`
- PH test (CSV): (missing) `C:/Users/grant/OneDrive/Documents/GitHub/bio-epi-portfolio/projects/clinical-trials/projects/ct03_longitudinal/tables/diagnostics_ph_test.csv`
- CT01 interaction OR (CSV): (missing) `C:/Users/grant/OneDrive/Documents/GitHub/bio-epi-portfolio/projects/clinical-trials/projects/ct03_longitudinal/tables/interaction_or_by_severity.csv`
- CT02 interaction HR (CSV): (missing) `C:/Users/grant/OneDrive/Documents/GitHub/bio-epi-portfolio/projects/clinical-trials/projects/ct03_longitudinal/tables/interaction_hr_by_severity.csv`

QC artifacts referenced:
- `C:/Users/grant/OneDrive/Documents/GitHub/bio-epi-portfolio/projects/clinical-trials/projects/ct03_longitudinal/results/QC_02_data_cleaning.txt`
- `C:/Users/grant/OneDrive/Documents/GitHub/bio-epi-portfolio/projects/clinical-trials/projects/ct03_longitudinal/results/QC_03_table1.txt` (if present)
- `C:/Users/grant/OneDrive/Documents/GitHub/bio-epi-portfolio/projects/clinical-trials/projects/ct03_longitudinal/results/QC_04_primary_analysis.txt`
- `C:/Users/grant/OneDrive/Documents/GitHub/bio-epi-portfolio/projects/clinical-trials/projects/ct03_longitudinal/results/QC_05_secondary_analyses.txt`
