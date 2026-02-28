# CT04 Results Packet

Generated: 2026-02-23 15:48:25.171198

## Tables (CSV)
- [Baseline characteristics](tables/Table_14_1_1_Baseline.csv)
- [PK parameters by dose](tables/Table_14_2_1_PK_Params.csv)
- [Dose proportionality model](tables/Table_14_3_1_Dose_Prop.csv)
- [Exposure–response model](tables/Table_14_4_1_Expo_Response.csv)
- [Safety summary](tables/Table_14_5_1_Safety.csv)

## Figures (PNG)
- [Mean concentration–time](figures/Figure_14_2_1_Mean_Conc_Time.png)
- [Individual profiles](figures/Figure_14_2_2_Indiv_Conc_Time.png)
- [Exposure–response curve](figures/Figure_14_4_1_Expo_Response_AUC.png)

## Models / Derived Data
- `models/05_models_dose_proportionality.rds`
- `models/05_models_exposure_response.rds`
- `models/04_pk_params.rds`

## Misc
- `misc/05_dose_proportionality_results.csv`
- `misc/05_exposure_response_results.csv`
- `misc/05_safety_by_dose_summary.csv`
- `misc/06_diagnostics_summary.txt`

## QC Artifacts
- `qc/QC_02_data_cleaning.txt`
- `qc/QC_04_primary_analysis.txt`
- `qc/QC_05_secondary_analyses.txt`
- `qc/QC_06_diagnostics.txt`

## Notes
- Primary exposure metric: AUC (Cmax evaluated as sensitivity).
- Dose proportionality assessed via log–log linear model excluding placebo.
- Exposure–response assessed with logistic regression of response on exposure.
- Safety summarized descriptively by AE and DLT by dose group.
