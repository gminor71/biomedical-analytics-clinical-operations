# src/05_secondary_analyses.R
# Purpose: Secondary / sensitivity analyses for CT03 (longitudinal biomarker)

in_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
qc_assert(file.exists(in_rds), "Missing analysis-ready dataset. Run 02_data_cleaning.R first.")
df <- readRDS(in_rds)

# ---- Longitudinal QC (long format) ----
required_cols <- c("usubjid", "trt", "age", "sex", "baseline_severity", "bmi", "visit_day", "biomarker")
missing_cols <- setdiff(required_cols, names(df))
qc_assert(length(missing_cols) == 0,
          paste0("QC: required columns missing: ", paste(missing_cols, collapse = ", ")))

# Treatment/sex expectations
qc_assert(is.factor(df$trt) && identical(levels(df$trt), c("Control", "Active")),
          "QC: trt must be factor with levels c('Control','Active').")
qc_assert(is.factor(df$sex), "QC: sex must be factor.")
qc_assert(is.numeric(df$visit_day) && all(df$visit_day >= 0, na.rm = TRUE),
          "QC: visit_day must be numeric and >= 0.")

# Long-format uniqueness: (usubjid, visit_day) should be unique
dup_keys <- df |>
  dplyr::count(usubjid, visit_day) |>
  dplyr::filter(n > 1)

qc_assert(nrow(dup_keys) == 0,
          paste0("QC: duplicate (usubjid, visit_day) rows detected. Example:\n",
                 paste(utils::capture.output(print(utils::head(dup_keys, 10))), collapse = "\n")))

model_cols <- c("usubjid","trt","age","sex","baseline_severity","bmi","visit_day","biomarker")

df_model <- df |>
  dplyr::mutate(time30 = visit_day / 30) |>
  dplyr::select(dplyr::all_of(model_cols), time30) |>
  tidyr::drop_na(trt, visit_day, age, sex, baseline_severity, bmi, biomarker)

qc_assert(nrow(df_model) > 0, "QC: No rows available for secondary models.")
qc_assert(dplyr::n_distinct(df_model$usubjid) > 1, "QC: Need >1 subject.")

# ---- Models ----
m_sens <- lme4::lmer(
  biomarker ~ trt * time30 + age + sex + bmi + (1 | usubjid),
  data = df_model, REML = FALSE
)

# Random slope model (may fail; don’t hard-stop)
m_rs <- tryCatch(
  lme4::lmer(
    biomarker ~ trt * time30 + age + sex + baseline_severity + bmi + (1 + time30 | usubjid),
    data = df_model, REML = FALSE
  ),
  error = function(e) NULL
)

# Effect modification on slope: severity x treatment x time
m_mod <- lme4::lmer(
  biomarker ~ trt * time30 * baseline_severity + age + sex + bmi + (1 | usubjid),
  data = df_model, REML = FALSE
)

# ---- Outputs ----
tbl_sens <- gtsummary::tbl_regression(m_sens) |>
  gtsummary::modify_header(label ~ "**Term**") |>
  gtsummary::bold_labels()

tbl_mod <- gtsummary::tbl_regression(m_mod) |>
  gtsummary::modify_header(label ~ "**Term**") |>
  gtsummary::bold_labels()

tbls <- list(tbl_sens, tbl_mod)
hdrs <- c("Sensitivity: Drop baseline severity", "Exploratory: Severity effect modification (trt*time*severity)")

if (!is.null(m_rs)) {
  tbl_rs <- gtsummary::tbl_regression(m_rs) |>
    gtsummary::modify_header(label ~ "**Term**") |>
    gtsummary::bold_labels()
  tbls <- append(tbls, list(tbl_rs))
  hdrs <- c(hdrs, "Exploratory: Random slope model (1 + time30 | usubjid)")
}

stacked <- gtsummary::tbl_stack(tbls, group_header = hdrs)

out_html <- file.path(PATHS$tables, "secondary_models.html")
gtsummary::as_gt(stacked) |>
  gt::gtsave(out_html)

models_path <- file.path(PATHS$results, "secondary_models.rds")
saveRDS(list(m_sens = m_sens, m_mod = m_mod, m_rs = m_rs), models_path)

# ---- QC report ----
qc_path <- file.path(PATHS$results, "QC_05_secondary_analyses.txt")

qc_write(c(
  "QC: 05_secondary_analyses.R (CT03 longitudinal)",
  paste0("Run at: ", Sys.time()),
  "",
  paste0("Rows used: ", nrow(df_model)),
  paste0("Subjects used: ", dplyr::n_distinct(df_model$usubjid)),
  "",
  paste0("Random slope model fit: ", ifelse(is.null(m_rs), "FAILED (skipped)", "OK")),
  "",
  paste0("Saved table: ", out_html),
  paste0("Saved models: ", models_path)
), qc_path)

message("✅ Secondary models saved: ", models_path)
message("✅ Secondary models table saved: ", out_html)
message("✅ QC report written: ", qc_path)
