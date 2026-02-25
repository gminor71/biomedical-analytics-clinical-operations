# src/04_primary_analysis.R
# Purpose: Derive PK parameters (AUC/Cmax/Tmax) and summarize exposure by dose group + QC

source("src/00_setup.R")

# Local AUC helper (trapezoid). Keep here to avoid hidden dependencies.
auc_trapz <- function(time, conc) {
  o <- order(time)
  t <- time[o]; c <- conc[o]
  sum(diff(t) * (head(c, -1) + tail(c, -1)) / 2, na.rm = TRUE)
}

in_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
stopifnot(file.exists(in_rds))

dat <- readRDS(in_rds)
subj <- dat$subj
pk   <- dat$pk_long

# ---- Derive PK parameters per subject ----
pk_params <- pk |>
  dplyr::arrange(usubjid, time) |>
  dplyr::group_by(usubjid, dosegrp, dose) |>
  dplyr::summarize(
    auc  = auc_trapz(time, conc),
    cmax = max(conc, na.rm = TRUE),
    tmax = time[which.max(conc)][1],
    .groups = "drop"
  ) |>
  dplyr::left_join(
    subj |>
      dplyr::select(usubjid, site, trt, age, sex, bmi, baseline_severity, resp_bin, ae_any, dlt),
    by = "usubjid"
  )

out_pk_params <- file.path(PATHS$data_proc, "04_pk_params.rds")
saveRDS(pk_params, out_pk_params)
message("✅ Saved: ", out_pk_params)

# ---- Primary summaries: exposure by dose group (for later TFLs) ----
summ_exposure <- pk_params |>
  dplyr::group_by(dosegrp) |>
  dplyr::summarize(
    n = dplyr::n(),
    auc_mean  = mean(auc, na.rm = TRUE),
    auc_sd    = sd(auc, na.rm = TRUE),
    cmax_mean = mean(cmax, na.rm = TRUE),
    cmax_sd   = sd(cmax, na.rm = TRUE),
    .groups = "drop"
  )

summ_path <- file.path(PATHS$results, "04_primary_exposure_summary.csv")
readr::write_csv(summ_exposure, summ_path)
message("✅ Saved: ", summ_path)

# ---- QC token ----
qc_path <- file.path(PATHS$results, "QC_04_primary_analysis.txt")
writeLines(
  c(
    "QC_04_primary_analysis: OK",
    paste0("04_pk_params.rds exists: ", file.exists(out_pk_params)),
    paste0("04_primary_exposure_summary.csv exists: ", file.exists(summ_path)),
    paste0("PK params rows: ", nrow(pk_params))
  ),
  qc_path
)
