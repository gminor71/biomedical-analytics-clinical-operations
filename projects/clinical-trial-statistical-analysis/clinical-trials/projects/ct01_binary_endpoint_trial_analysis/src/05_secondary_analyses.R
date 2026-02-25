# src/05_secondary_analyses.R
# Purpose: Secondary / sensitivity analyses for CT01 aligned to SAP
# 1) Sensitivity: remove baseline_severity from adjustment set
# 2) Exploratory: trt x baseline_severity interaction (effect modification)

source("src/00_setup.R")


# ---- Load analysis-ready dataset ----
in_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
qc_assert(file.exists(in_rds), "Missing analysis-ready dataset. Run 02_data_cleaning.R first.")
df <- readRDS(in_rds)

# ---- Base QC (fail fast) ----
required_cols <- c("usubjid", "trt", "age", "sex", "baseline_severity", "event_bin")
qc_base <- qc_dataset_cleaning(
  df,
  required_cols = required_cols,
  trt_col = "trt",
  endpoint_col = "event_bin"
)

qc_assert(is.factor(df$trt) && identical(levels(df$trt), c("Control", "Active")),
          "QC: trt must be factor with levels c('Control','Active').")
qc_assert(length(unique(na.omit(df$event_bin))) == 2,
          "QC: event_bin must contain both 0 and 1.")

# ---- Analysis set (complete-case for all variables used across secondary models) ----
vars_all <- c("event_bin", "trt", "age", "sex", "baseline_severity")
df_cc <- df |>
  dplyr::select(dplyr::all_of(vars_all)) |>
  tidyr::drop_na()

qc_assert(nrow(df_cc) > 0, "QC: No complete-case rows available for secondary models.")
qc_assert(length(unique(df_cc$event_bin)) == 2,
          "QC: After complete-case filtering, event_bin lacks variation.")

# ---- Fit secondary models ----
# Sensitivity model: remove baseline severity
m_sens <- glm(
  event_bin ~ trt + age + sex,
  data = df_cc,
  family = binomial()
)

# Exploratory interaction model
m_int <- glm(
  event_bin ~ trt + age + sex + baseline_severity + trt:baseline_severity,
  data = df_cc,
  family = binomial()
)

qc_assert(isTRUE(m_sens$converged), "QC: Sensitivity model did not converge.")
qc_assert(isTRUE(m_int$converged), "QC: Interaction model did not converge.")

# ---- Extract key treatment effects ----
# Sensitivity: trtActive
coef_sens <- summary(m_sens)$coefficients
qc_assert("trtActive" %in% rownames(coef_sens), "QC: 'trtActive' missing in sensitivity model.")
beta_s <- coef_sens["trtActive", "Estimate"]
se_s   <- coef_sens["trtActive", "Std. Error"]
or_s   <- exp(beta_s)
ci_s   <- exp(beta_s + c(-1, 1) * 1.96 * se_s)

# Interaction: treatment main effect at baseline_severity=0 is not interpretable as-is
# We report both:
# - interaction term OR per 1-unit increase in baseline severity (Active vs Control differential)
# - and compute OR at representative severities (e.g., 40, 50, 60)
coef_int <- summary(m_int)$coefficients
int_name <- "trtActive:baseline_severity"
qc_assert(int_name %in% rownames(coef_int), paste0("QC: '", int_name, "' missing in interaction model."))

beta_int <- coef_int[int_name, "Estimate"]
se_int   <- coef_int[int_name, "Std. Error"]
or_int   <- exp(beta_int)
ci_int   <- exp(beta_int + c(-1, 1) * 1.96 * se_int)

# OR(Active vs Control) at baseline severity values
sev_vals <- c(40, 50, 60)
b <- coef(m_int)
V <- vcov(m_int)

# Function to compute OR and 95% CI at a given severity (using delta method)
or_at_sev <- function(sev) {
  # log(OR) for Active vs Control at severity=sev:
  # beta_trtActive + beta_interaction * sev
  L <- rep(0, length(b))
  names(L) <- names(b)
  L["trtActive"] <- 1
  L[int_name] <- sev
  
  log_or <- sum(L * b)
  var_log_or <- as.numeric(t(L) %*% V %*% L)
  se_log_or <- sqrt(var_log_or)
  
  c(
    sev = sev,
    OR = exp(log_or),
    CI_low = exp(log_or - 1.96 * se_log_or),
    CI_high = exp(log_or + 1.96 * se_log_or)
  )
}

or_sev <- t(sapply(sev_vals, or_at_sev)) |> as.data.frame()
or_sev$sev <- as.integer(or_sev$sev)

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

# Also save the OR-at-severity table for interpretation
or_sev_path <- file.path(PATHS$tables, "interaction_or_by_severity.csv")
readr::write_csv(or_sev, or_sev_path)

# ---- Save model objects ----
models_path <- file.path(PATHS$results, "secondary_models.rds")
saveRDS(list(m_sens = m_sens, m_int = m_int, or_by_severity = or_sev), models_path)

# ---- QC report ----
qc_path <- file.path(PATHS$results, "QC_05_secondary_analyses.txt")

qc_write(c(
  "QC: 05_secondary_analyses.R (Secondary / Sensitivity Analyses)",
  paste0("Run at: ", Sys.time()),
  "",
  paste0("N (analysis-ready ITT): ", nrow(df)),
  paste0("N (complete-case used in secondary models): ", nrow(df_cc)),
  "",
  "N by treatment (analysis-ready ITT):",
  qc_fmt_table(qc_base$n_by_trt),
  "",
  "Event rate by treatment (analysis-ready ITT; sanity only):",
  paste(names(qc_base$event_rate_by_trt), round(qc_base$event_rate_by_trt, 4), sep = ": "),
  "",
  "Sensitivity model: event_bin ~ trt + age + sex",
  paste0("  OR (Active vs Control) = ", round(or_s, 4),
         " | 95% CI [", round(ci_s[1], 4), ", ", round(ci_s[2], 4), "]"),
  "",
  "Exploratory interaction model: event_bin ~ trt + age + sex + baseline_severity + trt:baseline_severity",
  paste0("  Interaction term OR per 1-unit severity = ", round(or_int, 4),
         " | 95% CI [", round(ci_int[1], 4), ", ", round(ci_int[2], 4), "]"),
  "",
  "OR(Active vs Control) at representative baseline severities (delta method):",
  qc_fmt_table(or_sev),
  "",
  paste0("Saved: ", out_html),
  paste0("Saved: ", or_sev_path),
  paste0("Saved: ", models_path)
), qc_path)

message("✅ Secondary models saved: ", models_path)
message("✅ Secondary model table saved: ", out_html)
message("✅ Interaction OR-by-severity saved: ", or_sev_path)
message("✅ QC report written: ", qc_path)
