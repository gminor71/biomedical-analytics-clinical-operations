# src/04_primary_analysis.R
# Purpose: Primary efficacy analysis for CT02 (time-to-event) aligned to SAP Week 0
# Model: Cox proportional hazards, adjusted for age, sex, baseline_severity

if (!exists("PATHS")) stop("PATHS not found. Run source('src/00_setup.R') first (or use 99_run_all.R).")
qc_assert(exists("qc_write"), "QC helpers not loaded. Run source('src/00_setup.R') first (or use 99_run_all.R).")

# ---- Load analysis-ready dataset ----
in_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
if (!file.exists(in_rds)) {
  stop("Missing analysis-ready dataset. Run 02_data_cleaning.R first.")
}
df <- readRDS(in_rds)

# ---- Required columns & basic QC (fail fast) ----
required_cols <- c("usubjid", "trt", "age", "sex", "baseline_severity", "time", "status")

# Use qc_dataset_cleaning for general structure checks (even though endpoint differs)
qc_base <- qc_dataset_cleaning(
  df,
  required_cols = required_cols,
  trt_col = "trt",
  endpoint_col = "status"
)

# Additional checks specific to modeling
qc_assert(is.factor(df$trt), "QC: trt must be a factor.")
qc_assert(identical(levels(df$trt), c("Control", "Active")),
          "QC: trt levels must be c('Control','Active') for correct reference group.")
qc_assert(is.numeric(df$age), "QC: age must be numeric.")
qc_assert(is.numeric(df$baseline_severity), "QC: baseline_severity must be numeric.")
qc_assert(is.factor(df$sex), "QC: sex must be a factor.")
qc_assert(is.numeric(df$time), "QC: time must be numeric.")
qc_assert(all(df$status %in% c(0L, 1L), na.rm = TRUE), "QC: status must be 0/1.")
qc_assert(all(df$time >= 0, na.rm = TRUE), "QC: time must be >= 0.")

# Ensure at least one event
qc_assert(sum(df$status, na.rm = TRUE) > 0, "QC: No events observed (status==1).")

# ---- Analysis set (complete case for prespecified covariates) ----
vars_model <- c("time", "status", "trt", "age", "sex", "baseline_severity")
df_model <- df %>% dplyr::select(dplyr::all_of(vars_model)) %>% tidyr::drop_na()

qc_assert(nrow(df_model) > 0, "QC: No complete-case rows available for primary Cox model.")
qc_assert(sum(df_model$status, na.rm = TRUE) > 0,
          "QC: After complete-case filtering, no events remain (status==1).")

# ---- Fit primary model ----
primary_model <- survival::coxph(
  survival::Surv(time, status) ~ trt + age + sex + baseline_severity,
  data = df_model,
  ties = "efron"
)

# ---- Extract prespecified treatment effect ----
coef_name <- "trtActive"
b <- coef(primary_model)
V <- vcov(primary_model)

qc_assert(coef_name %in% names(b),
          "QC: Expected coefficient 'trtActive' not found. Check trt coding/levels.")

beta <- as.numeric(b[coef_name])
se   <- as.numeric(sqrt(V[coef_name, coef_name]))

hr <- exp(beta)
ci <- exp(beta + c(-1, 1) * 1.96 * se)

# p-value from Wald test
z <- beta / se
p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)

# ---- Save model object ----
model_path <- file.path(PATHS$results, "primary_model.rds")
saveRDS(primary_model, model_path)

# ---- Create and save primary results table (HRs) ----
tbl_primary <-
  gtsummary::tbl_regression(
    primary_model,
    exponentiate = TRUE
  ) %>%
  gtsummary::modify_header(label ~ "**Term**") %>%
  gtsummary::bold_labels()

out_html <- file.path(PATHS$tables, "primary_model.html")
gtsummary::as_gt(tbl_primary) |>
  gt::gtsave(out_html)

# ---- QC report ----
qc_path <- file.path(PATHS$results, "QC_04_primary_analysis.txt")

n_by_trt <- table(df$trt)
events_by_trt <- with(df, tapply(status, trt, sum))
qc_assert(all(events_by_trt > 0, na.rm = TRUE),
          "QC: One treatment group has 0 events. Cox model may be unstable or not estimable.")
event_rate_by_trt <- with(df, tapply(status, trt, mean))
median_follow_by_trt <- with(df, tapply(time, trt, median))

qc_write(c(
  "QC: 04_primary_analysis.R (Primary Cox proportional hazards)",
  paste0("Run at: ", Sys.time()),
  "",
  "Model: Surv(time, status) ~ trt + age + sex + baseline_severity (Cox PH)",
  "",
  paste0("N (analysis-ready ITT): ", nrow(df)),
  paste0("N (complete-case used in model): ", nrow(df_model)),
  "",
  "N by treatment (analysis-ready ITT):",
  paste(paste(names(n_by_trt), as.integer(n_by_trt), sep = ": "), collapse = "\n"),
  "",
  "Events by treatment (analysis-ready ITT; sanity only):",
  paste(paste(names(events_by_trt), as.integer(events_by_trt), sep = ": "), collapse = "\n"),
  "",
  "Event rate by treatment (analysis-ready ITT; sanity only):",
  paste(paste(names(event_rate_by_trt), round(event_rate_by_trt, 4), sep = ": "), collapse ="\n"),
  "",
  "Median follow-up time by treatment (analysis-ready ITT; sanity only):",
  paste(paste(names(median_follow_by_trt), round(median_follow_by_trt, 1), sep = ": "), collapse = "\n"),
  "",
  "Primary treatment effect (Active vs Control):",
  paste0("  log(HR) = ", round(beta, 4)),
  paste0("  SE      = ", round(se, 4)),
  paste0("  z       = ", round(z, 4)),
  paste0("  p       = ", signif(p, 4)),
  paste0("  HR      = ", round(hr, 4)),
  paste0("  95% CI  = [", round(ci[1], 4), ", ", round(ci[2], 4), "]")
), qc_path)

message("✅ Primary Cox model saved to: ", model_path)
message("✅ Primary model table saved to: ", out_html)
message("✅ QC report written: ", qc_path)
