# CT03 Results Packet

Generated: 2026-02-06 16:19:16.158243

## Tables
- [Table 1 – Baseline](tables/table1_baseline.html)
- [Primary mixed model](tables/primary_model.html)
- [Secondary models](tables/secondary_models.html)

## Figures
- [Mean biomarker over time](figures/biomarker_mean_over_time.png)
- [Residuals vs fitted](figures/diag_resid_vs_fitted.png)
- [Q-Q residuals](figures/diag_resid_qq.png)
- [Random intercepts](figures/diag_ranef_intercepts.png)

## Misc CSVs
- [Residuals table](misc/diagnostics_residuals_summary.csv)
- [Estimated means by visit](misc/primary_emmeans_by_visit.csv)

## QC Artifacts
- `qc/QC_02_data_cleaning.txt`
- `qc/QC_03_table1.txt`
- `qc/QC_04_primary_analysis.txt`
- `qc/QC_05_secondary_analyses.txt`
- `qc/QC_06_diagnostics.txt`

## Notes
- Table 1 is baseline-only (one row per subject).
- Primary model: mixed model with trt-by-time interaction (time scaled per 30 days).
- Secondary models include sensitivity and exploratory random slopes / effect modification.
