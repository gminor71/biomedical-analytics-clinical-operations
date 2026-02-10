# src/05_secondary_analyses.R
# Purpose: Secondary / sensitivity analyses for CT02 (time-to-event) aligned to SAP
# 1) Sensitivity: remove baseline_severity from adjustment set
# 2) Exploratory: trt x baseline_severity interaction (effect modification)

if (!exists("PATHS")) stop("PATHS not found. Run source('src/00_setup.R') first (or use 99_run_all.R).")
qc_assert(exists("qc_write"), "QC helpers not loaded. Run source('src/00_setup.R') first (or use 99_run_all.R).")


# ---- Load analysis-ready dataset ----
in_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
qc_assert(file.exists(in_rds), "Missing analysis-ready dataset. Run 02_data_cleaning.R first.")
df <- readRDS(in_rds)

# ---- Base QC (fail fast) ----
required_cols <- c("usubjid", "trt", "age", "sex", "baseline_severity", "time", "status")
qc_base <- qc_dataset_cleaning(
  df,
  required_cols = required_cols,
  trt_col = "trt",
  endpoint_col = "status"
)

qc_assert(is.factor(df$trt) && identical(levels(df$trt), c("Control", "Active")),
          "QC: trt must be factor with levels c('Control','Active').")
qc_assert(all(df$status %in% c(0L, 1L), na.rm = TRUE), "QC: status must be 0/1.")
qc_assert(all(df$time >= 0, na.rm = TRUE), "QC: time must be >= 0.")
qc_assert(sum(df$status, na.rm = TRUE) > 0, "QC: No events in analysis-ready dataset.")

# ---- Analysis set (complete-case for all variables used across secondary models) ----
vars_all <- c("time", "status", "trt", "age", "sex", "baseline_severity")
df_cc <- df |>
  dplyr::select(dplyr::all_of(vars_all)) |>
  tidyr::drop_na()

qc_assert(nrow(df_cc) > 0, "QC: No complete-case rows available for secondary models.")
qc_assert(sum(df_cc$status, na.rm = TRUE) > 0,
          "QC: After complete-case filtering, no events remain (status==1).")

# ---- Fit secondary Cox models ----
m_sens <- survival::coxph(
  survival::Surv(time, status) ~ trt + age + sex,
  data = df_cc,
  ties = "efron"
)

m_int <- survival::coxph(
  survival::Surv(time, status) ~ trt + age + sex + baseline_severity + trt:baseline_severity,
  data = df_cc,
  ties = "efron"
)

qc_assert(!any(is.na(coef(m_sens))), "QC: Sensitivity Cox model has NA coefficients.")
qc_assert(!any(is.na(coef(m_int))),  "QC: Interaction Cox model has NA coefficients.")

# ---- Extract key effects (Sensitivity) ----
coef_sens <- coef(m_sens)
V_sens <- vcov(m_sens)

qc_assert("trtActive" %in% names(coef_sens), "QC: 'trtActive' missing in sensitivity Cox model.")
beta_s <- as.numeric(coef_sens["trtActive"])
se_s   <- as.numeric(sqrt(V_sens["trtActive", "trtActive"]))
hr_s   <- exp(beta_s)
ci_s   <- exp(beta_s + c(-1, 1) * 1.96 * se_s)

# ---- Extract interaction effect and HR at severity values (Exploratory) ----
coef_int <- coef(m_int)
V_int <- vcov(m_int)

int_name <- "trtActive:baseline_severity"
qc_assert(int_name %in% names(coef_int),
          paste0("QC: '", int_name, "' missing in interaction Cox model."))

beta_int <- as.numeric(coef_int[int_name])
se_int   <- as.numeric(sqrt(V_int[int_name, int_name]))
hr_int   <- exp(beta_int)
ci_int   <- exp(beta_int + c(-1, 1) * 1.96 * se_int)

# HR(Active vs Control) at representative severities (delta method)
sev_vals <- c(40, 50, 60)
b <- coef_int
V <- V_int

hr_at_sev <- function(sev) {
  # log(HR) for Active vs Control at severity=sev:
  # beta_trtActive + beta_interaction * sev
  L <- rep(0, length(b))
  names(L) <- names(b)
  L["trtActive"] <- 1
  L[int_name] <- sev
  
  log_hr <- sum(L * b)
  var_log_hr <- as.numeric(t(L) %*% V %*% L)
  se_log_hr <- sqrt(var_log_hr)
  
  c(
    sev = sev,
    HR = exp(log_hr),
    CI_low = exp(log_hr - 1.96 * se_log_hr),
    CI_high = exp(log_hr + 1.96 * se_log_hr)
  )
}

hr_sev <- t(sapply(sev_vals, hr_at_sev)) |> as.data.frame()
hr_sev$sev <- as.integer(hr_sev$sev)

# ---- Create summary table output ----
tbl_sens <- gtsummary::tbl_regression(m_sens, exponentiate = TRUE) |>
  gtsummary::modify_header(label ~ "**Term**") |>
  gtsummary::bold_labels()

tbl_int <- gtsummary::tbl_regression(m_int, exponentiate = TRUE) |>
  gtsummary::modify_header(label ~ "**Term**") |>
  gtsummary::bold_labels()

stacked <-
  gtsummary::tbl_stack(
    list(tbl_sens, tbl_int),
    group_header = c("Sensitivity: Adjusted (Age + Sex)", "Exploratory: + Baseline Severity and Interaction")
  )

out_html <- file.path(PATHS$tables, "secondary_models.html")
gtsummary::as_gt(stacked) |>
  gt::gtsave(out_html)

# Save HR-at-severity table for interpretation
hr_sev_path <- file.path(PATHS$tables, "interaction_hr_by_severity.csv")
readr::write_csv(hr_sev, hr_sev_path)

# ---- Save model objects ----
models_path <- file.path(PATHS$results, "secondary_models.rds")
saveRDS(list(m_sens = m_sens, m_int = m_int, hr_by_severity = hr_sev), models_path)

# ---- QC report ----
qc_path <- file.path(PATHS$results, "QC_05_secondary_analyses.txt")

n_by_trt <- table(df$trt)
events_by_trt <- with(df, tapply(status, trt, sum))
qc_assert(all(events_by_trt > 0, na.rm = TRUE),
          "QC: One treatment group has 0 events. Secondary Cox models may be unstable or not estimable.")
event_rate_by_trt <- with(df, tapply(status, trt, mean))
median_follow_by_trt <- with(df, tapply(time, trt, median))

qc_write(c(
  "QC: 05_secondary_analyses.R (Secondary / Sensitivity Analyses - Cox PH)",
  paste0("Run at: ", Sys.time()),
  "",
  paste0("N (analysis-ready ITT): ", nrow(df)),
  paste0("N (complete-case used in secondary models): ", nrow(df_cc)),
  "",
  "N by treatment (analysis-ready ITT):",
  paste(names(n_by_trt), as.integer(n_by_trt), sep = ": "),
  "",
  "Events by treatment (analysis-ready ITT; sanity only):",
  paste(names(events_by_trt), as.integer(events_by_trt), sep = ": "),
  "",
  "Event rate by treatment (analysis-ready ITT; sanity only):",
  paste(names(event_rate_by_trt), round(event_rate_by_trt, 4), sep = ": "),
  "",
  "Median follow-up time by treatment (analysis-ready ITT; sanity only):",
  paste(names(median_follow_by_trt), round(median_follow_by_trt, 1), sep = ": "),
  "",
  "Sensitivity Cox model: Surv(time,status) ~ trt + age + sex",
  paste0("  HR (Active vs Control) = ", round(hr_s, 4),
         " | 95% CI [", round(ci_s[1], 4), ", ", round(ci_s[2], 4), "]"),
  "",
  "Exploratory interaction Cox model: ... + baseline_severity + trt:baseline_severity",
  paste0("  Interaction term HR per 1-unit severity = ", round(hr_int, 4),
         " | 95% CI [", round(ci_int[1], 4), ", ", round(ci_int[2], 4), "]"),
  "",
  "HR(Active vs Control) at representative baseline severities (delta method):",
  qc_fmt_table(hr_sev),
  "",
  paste0("Saved: ", out_html),
  paste0("Saved: ", hr_sev_path),
  paste0("Saved: ", models_path)
), qc_path)

message("✅ Secondary models saved: ", models_path)
message("✅ Secondary model table saved: ", out_html)
message("✅ Interaction HR-by-severity saved: ", hr_sev_path)
message("✅ QC report written: ", qc_path)
