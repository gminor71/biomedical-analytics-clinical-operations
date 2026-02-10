# src/04_primary_analysis.R
# Purpose: Primary efficacy analysis for CT03 (longitudinal biomarker) aligned to SAP Week 0
# Model: Linear mixed model with trt x time, adjusted for age, sex, baseline_severity, bmi
# Random effect: subject-level random intercept

# ---- Load analysis-ready dataset ----
in_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
qc_assert(file.exists(in_rds), "Missing analysis-ready dataset. Run 02_data_cleaning.R first.")
df <- readRDS(in_rds)
# ---- Required columns & longitudinal QC ----
required_cols <- c("usubjid", "trt", "age", "sex", "baseline_severity", "bmi", "visit_day", "biomarker")
missing_cols <- setdiff(required_cols, names(df))
qc_assert(length(missing_cols) == 0,
          paste0("QC: missing required columns: ", paste(missing_cols, collapse = ", ")))

# Longitudinal key must be unique: (usubjid, visit_day)
key <- paste(df$usubjid, df$visit_day, sep = "||")
qc_assert(!anyDuplicated(key),
          "QC: duplicate (usubjid, visit_day) detected.")

# Type / value expectations
qc_assert(is.factor(df$trt) && identical(levels(df$trt), c("Control", "Active")),
          "QC: trt must be factor with levels c('Control','Active').")
qc_assert(is.factor(df$sex), "QC: sex must be factor.")
qc_assert(is.numeric(df$visit_day), "QC: visit_day must be numeric.")
qc_assert(all(df$visit_day >= 0, na.rm = TRUE), "QC: visit_day must be >= 0.")
qc_assert(is.numeric(df$biomarker), "QC: biomarker must be numeric.")


# ---- Construct analysis set ----
# Mixed models can handle missing outcomes; we drop rows with missing covariates/time or missing outcome
vars_model <- c("usubjid", "trt", "visit_day", "biomarker", "age", "sex", "baseline_severity", "bmi")
df_model <- df |>
  dplyr::select(dplyr::all_of(vars_model)) |>
  tidyr::drop_na(trt, visit_day, age, sex, baseline_severity, bmi) |>
  dplyr::filter(!is.na(biomarker))

qc_assert(nrow(df_model) > 0, "QC: No rows available after filtering for model inputs/outcome.")
qc_assert(dplyr::n_distinct(df_model$usubjid) > 1, "QC: Need >1 subject for mixed model.")
qc_assert(length(unique(df_model$visit_day)) > 1, "QC: Need >1 visit day for longitudinal model.")

# Scale time: per 30 days
df_model <- df_model |>
  dplyr::mutate(time30 = visit_day / 30)

# ---- Fit primary mixed model ----
# Primary estimand: trtActive:time30 (difference in slope per 30 days)
primary_model <- lme4::lmer(
  biomarker ~ trt * time30 + age + sex + baseline_severity + bmi + (1 | usubjid),
  data = df_model,
  REML = FALSE
)

# ---- Extract prespecified interaction effect ----
# Use lmerTest if available for p-values; otherwise compute Wald z approx
coef_tab <- NULL
p_int <- NA_real_

if (requireNamespace("lmerTest", quietly = TRUE) && inherits(primary_model, "lmerModLmerTest")) {
  coef_tab <- summary(primary_model)$coefficients
  term <- "trtActive:time30"
  qc_assert(term %in% rownames(coef_tab), "QC: Expected term trtActive:time30 not found.")
  beta_int <- coef_tab[term, "Estimate"]
  se_int   <- coef_tab[term, "Std. Error"]
  p_int    <- coef_tab[term, "Pr(>|t|)"]
} else {
  b <- lme4::fixef(primary_model)
  V <- as.matrix(vcov(primary_model))
  term <- "trtActive:time30"
  qc_assert(term %in% names(b), "QC: Expected term trtActive:time30 not found.")
  beta_int <- as.numeric(b[term])
  se_int   <- as.numeric(sqrt(V[term, term]))
  z <- beta_int / se_int
  p_int <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
}

# ---- Save model object ----
model_path <- file.path(PATHS$results, "primary_model.rds")
saveRDS(primary_model, model_path)

# ---- Create primary results table ----
tbl_primary <- gtsummary::tbl_regression(primary_model) |>
  gtsummary::modify_header(label ~ "**Estimate (SE)**") |>
  gtsummary::bold_labels()

out_html <- file.path(PATHS$tables, "primary_model.html")
gtsummary::as_gt(tbl_primary) |>
  gt::gtsave(out_html)

# ---- Estimated marginal means at prespecified days (trial-style output) ----
days_star <- c(0, 30, 90, 180, 365)
qc_assert(!anyDuplicated(days_star), "QC: days_star contains duplicates.")

emm_path <- file.path(PATHS$tables, "primary_emmeans_by_visit.csv")

if (requireNamespace("emmeans", quietly = TRUE)) {
  
  # Model uses time30, so estimate at time30 = day/30
  em <- emmeans::emmeans(
    primary_model,
    ~ trt | time30,
    at = list(time30 = days_star / 30)
  )
  
  em_tbl <- as.data.frame(em)
  em_tbl$visit_day <- em_tbl$time30 * 30
  em_tbl$time30 <- NULL
  
  readr::write_csv(em_tbl, emm_path)
}

# ---- QC report ----
qc_path <- file.path(PATHS$results, "QC_04_primary_analysis.txt")

n_subj <- dplyr::n_distinct(df$usubjid)
n_subj_model <- dplyr::n_distinct(df_model$usubjid)
n_rows_model <- nrow(df_model)

qc_write(c(
  "QC: 04_primary_analysis.R (Primary longitudinal mixed model)",
  paste0("Run at: ", Sys.time()),
  "",
  "Model: biomarker ~ trt*time30 + age + sex + baseline_severity + bmi + (1|usubjid)",
  "Primary estimand: trtActive:time30 (difference in slope per 30 days)",
  "",
  paste0("Subjects (analysis-ready): ", n_subj),
  paste0("Subjects (used in model): ", n_subj_model),
  paste0("Rows (used in model): ", n_rows_model),
  "",
  "Key effect:",
  paste0("  beta(trtActive:time30) = ", round(beta_int, 4)),
  paste0("  SE = ", round(se_int, 4)),
  paste0("  p  = ", signif(p_int, 4)),
  "",
  paste0("Saved model: ", model_path),
  paste0("Saved table: ", out_html),
  paste0("Saved emmeans (if available): ", emm_path)
), qc_path)

message("✅ Primary mixed model saved: ", model_path)
message("✅ Primary model table saved: ", out_html)
message("✅ QC report written: ", qc_path)
