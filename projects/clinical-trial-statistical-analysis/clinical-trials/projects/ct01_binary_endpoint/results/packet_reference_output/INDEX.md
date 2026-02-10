# CT01 Results Packet

Generated: 2026-01-23 11:19:54.077788

## Tables
- [Table 1 – Baseline](tables/table1_baseline.html)
- [Primary model – ORs](tables/primary_model.html)
- [Secondary models](tables/secondary_models.html)

## Figures (Diagnostics)
- [Residuals vs fitted](figures/diag_residuals_vs_fitted.png)
- [Q-Q standardized deviance residuals](figures/diag_std_residuals_qq.png)
- [Cook's distance](figures/diag_cooks_distance.png)
- [Leverage](figures/diag_leverage.png)

## Key CSVs
- [Interaction OR by baseline severity](misc/interaction_or_by_severity.csv)
- [Influence summary](misc/diagnostics_influence_summary.csv)
- [VIF / GVIF](misc/diagnostics_vif.csv)

## Model Objects (RDS)
- `models/primary_model.rds`
- `models/secondary_models.rds`

## QC Artifacts
- `qc/QC_02_data_cleaning.txt`
- `qc/QC_03_table1.txt`
- `qc/QC_04_primary_analysis.txt`
- `qc/QC_05_secondary_analyses.txt`
- `qc/QC_06_diagnostics.txt`

## Notes
- Table 1 is descriptive only (no p-values).
- Primary model aligns to SAP: logistic regression adjusted for age, sex, baseline severity.
- Secondary includes sensitivity (drop baseline severity) and exploratory interaction.
