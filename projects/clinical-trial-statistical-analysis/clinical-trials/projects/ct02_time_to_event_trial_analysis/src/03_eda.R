# src/03_eda.R
# Purpose: Baseline descriptive statistics (Table 1) for CT02
# SAP-aligned: descriptive only, no hypothesis testing

# NOTE: 00_setup.R is sourced by 99_run_all.R

# ---- Load analysis-ready dataset ----
in_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
if (!file.exists(in_rds)) {
  stop("Missing analysis-ready dataset. Run 02_data_cleaning.R first.")
}
df <- readRDS(in_rds)

# ---- Variables for Table 1 (prespecified) ----
tbl_vars <- c("age", "sex", "baseline_severity", "bmi")

missing_tbl_vars <- setdiff(tbl_vars, names(df))
if (length(missing_tbl_vars) > 0) {
  stop("Table 1 variables missing from dataset: ", paste(missing_tbl_vars, collapse = ", "))
}

# ---- CT02 endpoint sanity variables ----
qc_assert(all(c("time", "status") %in% names(df)), "QC: CT02 requires columns time and status.")
qc_assert(all(df$status %in% c(0L, 1L), na.rm = TRUE), "QC: status must be 0/1.")
qc_assert(all(df$time >= 0, na.rm = TRUE), "QC: time must be >= 0.")

# ---- QC: Table 1 spot checks ----
qc1 <- qc_table1(df, tbl_vars = tbl_vars, trt_col = "trt")
qc_path <- file.path(PATHS$results, "QC_03_table1.txt")

qc_write(c(
  "QC: 03_eda.R (Table 1)",
  paste0("Run at: ", Sys.time()),
  "",
  "N by treatment (must match Table 1 headers):",
  qc_fmt_table(qc1$n_by_trt),
  "",
  "Continuous spot-checks (mean, sd):",
  if (length(qc1$cont_checks) == 0) "None" else paste(vapply(qc1$cont_checks, qc_fmt_table, ""), collapse = "\n\n"),
  "",
  "Categorical spot-checks (counts, %):",
  if (length(qc1$cat_checks) == 0) "None" else paste(vapply(qc1$cat_checks, function(z) {
    paste0("Var: ", z$var, "\nCounts:\n", qc_fmt_table(z$counts), "\nPercent:\n", qc_fmt_table(z$perc))
  }, ""), collapse = "\n\n")
), qc_path)

message("✅ QC report written: ", qc_path)

# ---- Create Table 1 ----
tbl1 <-
  gtsummary::tbl_summary(
    data = df,
    by = "trt",
    include = all_of(tbl_vars),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  gtsummary::add_overall() %>%
  gtsummary::modify_header(label ~ "**Characteristic**") %>%
  gtsummary::bold_labels()

# ---- Save outputs ----
out_html <- file.path(PATHS$tables, "table1_baseline.html")
out_rds  <- file.path(PATHS$results, "table1_baseline.rds")
eda_txt  <- file.path(PATHS$results, "03_eda_summary.txt")

gtsummary::as_gt(tbl1) |>
  gt::gtsave(out_html)

saveRDS(tbl1, out_rds)

# ---- Optional: quick EDA summaries (non-inferential) ----
eda_lines <- c(
  paste0("EDA run at: ", Sys.time()),
  "",
  "Sample size by treatment:",
  paste(capture.output(print(table(df$trt))), collapse = "\n"),
  "",
  "Events by treatment (sanity only):",
  paste(capture.output(with(df, tapply(status, trt, sum))), collapse = "\n"),
  "",
  "Median follow-up by treatment (sanity only):",
  paste(capture.output(with(df, tapply(time, trt, median))), collapse = "\n")
)

writeLines(eda_lines, eda_txt)

message("✅ Table 1 saved to: ", out_html)
message("✅ Table 1 object saved to: ", out_rds)
message("✅ EDA summary saved to: ", eda_txt)
