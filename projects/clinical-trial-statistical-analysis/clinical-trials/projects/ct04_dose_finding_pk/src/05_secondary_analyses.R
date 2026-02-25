# src/05_secondary_analyses.R
# Purpose: Dose proportionality, exposure-response, and safety summaries + QC

source("src/00_setup.R")

pk_params_path <- file.path(PATHS$data_proc, "04_pk_params.rds")
stopifnot(file.exists(pk_params_path))

pkp <- readRDS(pk_params_path)

# ---- Helper to extract coefficient summary ----
coef_row <- function(fit, term) {
  s <- summary(fit)$coefficients
  if (!term %in% rownames(s)) return(NULL)
  est <- s[term, "Estimate"]
  se  <- s[term, "Std. Error"]
  
  p <- NA_real_
  if ("Pr(>|t|)" %in% colnames(s)) p <- s[term, "Pr(>|t|)"]
  if (is.na(p) && "Pr(>|z|)" %in% colnames(s)) p <- s[term, "Pr(>|z|)"]
  
  ci <- suppressWarnings(confint(fit))
  lo <- if (!is.null(ci) && term %in% rownames(ci)) ci[term, 1] else NA_real_
  hi <- if (!is.null(ci) && term %in% rownames(ci)) ci[term, 2] else NA_real_
  
  tibble::tibble(term = term, estimate = est, se = se, ci_low = lo, ci_high = hi, p_value = p)
}

# ---- 1) Dose proportionality (exclude placebo: dose == 0) ----
dp_dat <- pkp |> dplyr::filter(dose > 0)

fit_dp_auc  <- stats::lm(log(auc)  ~ log(dose), data = dp_dat)
fit_dp_cmax <- stats::lm(log(cmax) ~ log(dose), data = dp_dat)

dp_res <- dplyr::bind_rows(
  coef_row(fit_dp_auc,  "log(dose)")  |> dplyr::mutate(metric = "AUC",  model = "log(AUC) ~ log(Dose)"),
  coef_row(fit_dp_cmax, "log(dose)")  |> dplyr::mutate(metric = "Cmax", model = "log(Cmax) ~ log(Dose)")
) |>
  dplyr::select(metric, model, estimate, se, ci_low, ci_high, p_value) |>
  dplyr::rename(slope_beta = estimate)

dp_out <- file.path(PATHS$results, "05_dose_proportionality_results.csv")
readr::write_csv(dp_res, dp_out)
message("✅ Saved: ", dp_out)

dp_models_out <- file.path(PATHS$results, "05_models_dose_proportionality.rds")
saveRDS(list(fit_dp_auc = fit_dp_auc, fit_dp_cmax = fit_dp_cmax), dp_models_out)
message("✅ Saved: ", dp_models_out)

# ---- 2) Exposure–response (primary: AUC; sensitivity: Cmax) ----
fit_er_auc  <- stats::glm(resp_bin ~ auc,  data = pkp, family = stats::binomial())
fit_er_cmax <- stats::glm(resp_bin ~ cmax, data = pkp, family = stats::binomial())

er_auc_row  <- coef_row(fit_er_auc,  "auc")  |> dplyr::mutate(metric = "AUC",  model = "logit(P(Response)) ~ AUC")
er_cmax_row <- coef_row(fit_er_cmax, "cmax") |> dplyr::mutate(metric = "Cmax", model = "logit(P(Response)) ~ Cmax")

er_res <- dplyr::bind_rows(er_auc_row, er_cmax_row) |>
  dplyr::mutate(
    odds_ratio = exp(estimate),
    or_ci_low  = exp(ci_low),
    or_ci_high = exp(ci_high)
  ) |>
  dplyr::select(metric, model, estimate, se, ci_low, ci_high, odds_ratio, or_ci_low, or_ci_high, p_value)

er_out <- file.path(PATHS$results, "05_exposure_response_results.csv")
readr::write_csv(er_res, er_out)
message("✅ Saved: ", er_out)

er_models_out <- file.path(PATHS$results, "05_models_exposure_response.rds")
saveRDS(list(fit_er_auc = fit_er_auc, fit_er_cmax = fit_er_cmax), er_models_out)
message("✅ Saved: ", er_models_out)

# ---- 3) Safety by dose group (descriptive) ----
safety_res <- pkp |>
  dplyr::group_by(dosegrp) |>
  dplyr::summarize(
    n = dplyr::n(),
    ae_any_n   = sum(ae_any == 1, na.rm = TRUE),
    ae_any_pct = mean(ae_any == 1, na.rm = TRUE) * 100,
    dlt_n      = sum(dlt == 1, na.rm = TRUE),
    dlt_pct    = mean(dlt == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  )

safety_out <- file.path(PATHS$results, "05_safety_by_dose_summary.csv")
readr::write_csv(safety_res, safety_out)
message("✅ Saved: ", safety_out)

# ---- Brief console summary ----
message("\nDose proportionality slopes (beta):")
print(dp_res)

message("\nExposure-response (odds ratios):")
print(er_res |> dplyr::select(metric, odds_ratio, or_ci_low, or_ci_high, p_value))

message("\nSafety summary by dose group:")
print(safety_res)

# ---- QC token ----
qc_path <- file.path(PATHS$results, "QC_05_secondary_analyses.txt")
writeLines(
  c(
    "QC_05_secondary_analyses: OK",
    paste0("05_dose_proportionality_results.csv exists: ", file.exists(dp_out)),
    paste0("05_exposure_response_results.csv exists: ", file.exists(er_out)),
    paste0("05_safety_by_dose_summary.csv exists: ", file.exists(safety_out)),
    paste0("Dose proportionality model RDS exists: ", file.exists(dp_models_out)),
    paste0("Exposure-response model RDS exists: ", file.exists(er_models_out))
  ),
  qc_path
)
