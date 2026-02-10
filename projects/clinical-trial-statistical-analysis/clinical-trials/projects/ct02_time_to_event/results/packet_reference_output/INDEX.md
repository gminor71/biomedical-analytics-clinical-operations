# CT02 Results Packet

Generated: 2026-02-04 17:52:08.187658

## Tables
- [Table 1 – Baseline](tables/table1_baseline.html)
- [Primary Cox model – HRs](tables/primary_model.html)
- [Secondary Cox models](tables/secondary_models.html)

## Figures
## Figures
- [Kaplan–Meier curve](figures/km_by_trt.png)
- [Schoenfeld residuals: trt (PH check)](figures/ph_schoenfeld_trt.png)

## Key CSVs
- [Interaction HR by baseline severity](misc/interaction_hr_by_severity.csv)
- [Proportional hazards test (cox.zph)](misc/diagnostics_ph_test.csv)
- [Influence summary (dfbeta)](misc/diagnostics_influence_summary.csv)

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
- Primary model aligns to SAP: Cox proportional hazards adjusted for age, sex, baseline severity.
- Secondary includes sensitivity (drop baseline severity) and exploratory interaction.
