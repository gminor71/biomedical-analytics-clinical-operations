# src/04_primary_analysis.R
# Purpose: Primary efficacy analysis for CT01 (binary endpoint) aligned to SAP Week 0
# Model: Logistic regression, adjusted for age, sex, baseline_severity

source("src/00_setup.R")


# ---- Load analysis-ready dataset ----
in_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
if (!file.exists(in_rds)) {
  stop("Missing analysis-ready dataset. Run 02_data_cleaning.R first.")
}

df <- readRDS(in_rds)

# ---- Required columns & basic QC (fail fast) ----
required_cols <- c("usubjid", "trt", "age", "sex", "baseline_severity", "event_bin")
qc <- qc_dataset_cleaning(
  df,
  required_cols = required_cols,
  trt_col = "trt",
  endpoint_col = "event_bin"
)

# Additional checks specific to modeling
qc_assert(is.factor(df$trt), "QC: trt must be a factor.")
qc_assert(identical(levels(df$trt), c("Control", "Active")),
          "QC: trt levels must be c('Control','Active') for correct reference group.")
qc_assert(is.numeric(df$age), "QC: age must be numeric.")
qc_assert(is.numeric(df$baseline_severity), "QC: baseline_severity must be numeric.")
qc_assert(is.factor(df$sex), "QC: sex must be a factor.")


# Ensure endpoint has variation (model can't fit otherwise)
qc_assert(length(unique(na.omit(df$event_bin))) == 2,
          "QC: event_bin must contain both 0 and 1 in the analysis set.")

# ---- Analysis set (complete case for prespecified covariates) ----
vars_model <- c("event_bin", "trt", "age", "sex", "baseline_severity")
df_model <- df %>% dplyr::select(all_of(vars_model)) %>% tidyr::drop_na()

qc_assert(nrow(df_model) > 0, "QC: No complete-case rows available for primary model.")
qc_assert(length(unique(df_model$event_bin)) == 2,
          "QC: After complete-case filtering, event_bin lacks variation (all 0s or all 1s).")

# ---- Fit primary model ----
primary_model <- glm(
  event_bin ~ trt + age + sex + baseline_severity,
  data = df_model,
  family = binomial()
)

# ---- Model convergence check ----
qc_assert(isTRUE(primary_model$converged), "QC: glm did not converge.")

# ---- Extract prespecified treatment effect ----
# trtActive is the coefficient comparing Active vs Control (reference)
coef_name <- "trtActive"
coefs <- summary(primary_model)$coefficients
qc_assert(coef_name %in% rownames(coefs),
          "QC: Expected coefficient 'trtActive' not found. Check trt coding/levels.")

beta <- coefs[coef_name, "Estimate"]
se   <- coefs[coef_name, "Std. Error"]
z    <- coefs[coef_name, "z value"]
p    <- coefs[coef_name, "Pr(>|z|)"]

or  <- exp(beta)
ci  <- exp(beta + c(-1, 1) * 1.96 * se)

# ---- Save model object ----
model_path <- file.path(PATHS$results, "primary_model.rds")
saveRDS(primary_model, model_path)

# ---- Create and save primary results table (ORs) ----
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

qc_write(c(
  "QC: 04_primary_analysis.R (Primary logistic regression)",
  paste0("Run at: ", Sys.time()),
  "",
  "Model: event_bin ~ trt + age + sex + baseline_severity (binomial/logit)",
  "",
  paste0("N (analysis-ready ITT): ", nrow(df)),
  paste0("N (complete-case used in model): ", nrow(df_model)),
  "",
  "N by treatment (analysis-ready ITT):",
  qc_fmt_table(qc$n_by_trt),
  "",
  "Event rate by treatment (analysis-ready ITT; sanity only):",
  paste(names(qc$event_rate_by_trt), round(qc$event_rate_by_trt, 4), sep = ": "),
  "",
  "Primary treatment effect (Active vs Control):",
  paste0("  log(OR) = ", round(beta, 4)),
  paste0("  SE      = ", round(se, 4)),
  paste0("  z       = ", round(z, 4)),
  paste0("  p       = ", signif(p, 4)),
  paste0("  OR      = ", round(or, 4)),
  paste0("  95% CI  = [", round(ci[1], 4), ", ", round(ci[2], 4), "]"),
  "",
  "Model fit notes:",
  paste0("  Converged: ", primary_model$converged),
  paste0("  AIC: ", round(AIC(primary_model), 2))
), qc_path)

message("✅ Primary model saved to: ", model_path)
message("✅ Primary model table saved to: ", out_html)
message("✅ QC report written: ", qc_path)

