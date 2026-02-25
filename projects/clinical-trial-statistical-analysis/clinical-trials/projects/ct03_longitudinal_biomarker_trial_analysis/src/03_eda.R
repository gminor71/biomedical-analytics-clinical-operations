# src/03_eda.R
# Purpose: Baseline descriptive statistics (Table 1) + simple longitudinal EDA for CT03
# SAP-aligned: descriptive only, no hypothesis testing

# ---- Load analysis-ready dataset ----
in_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
if (!file.exists(in_rds)) {
  stop("Missing analysis-ready dataset. Run 02_data_cleaning.R first.")
}
df <- readRDS(in_rds)

# ---- Baseline-only for Table 1 (avoid double-counting subjects) ----
df_bl <- df %>% dplyr::filter(visit_day == 0)

qc_assert(nrow(df_bl) > 0, "QC: No baseline rows (visit_day==0) found for Table 1.")
qc_assert(dplyr::n_distinct(df_bl$usubjid) == nrow(df_bl),
          "QC: Baseline dataset has duplicate subjects (multiple baseline rows).")

tbl_vars <- c("age", "sex", "baseline_severity", "bmi")
missing_tbl_vars <- setdiff(tbl_vars, names(df_bl))
if (length(missing_tbl_vars) > 0) {
  stop("Table 1 variables missing from baseline dataset: ", paste(missing_tbl_vars, collapse = ", "))
}

qc1 <- qc_table1(df_bl, tbl_vars = tbl_vars, trt_col = "trt")
qc_path <- file.path(PATHS$results, "QC_03_table1.txt")

qc_write(c(
  paste0("QC: 03_eda.R (CT03 Table 1 baseline-only)"),
  paste0("Run at: ", Sys.time()),
  "",
  "N by treatment (baseline; must match Table 1 headers):",
  qc_fmt_table(qc1$n_by_trt)
), qc_path)

message("✅ QC report written: ", qc_path)

# ---- Create Table 1 (baseline only) ----
tbl1 <-
  gtsummary::tbl_summary(
    data = df_bl,
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

out_html <- file.path(PATHS$tables, "table1_baseline.html")
out_rds  <- file.path(PATHS$results, "table1_baseline.rds")

gtsummary::as_gt(tbl1) |>
  gt::gtsave(out_html)
saveRDS(tbl1, out_rds)

message("✅ Table 1 saved to: ", out_html)

# ---- Longitudinal EDA: mean biomarker over time by treatment ----
df_sum <- df %>%
  dplyr::group_by(trt, visit_day) %>%
  dplyr::summarise(
    mean = mean(biomarker, na.rm = TRUE),
    n = sum(!is.na(biomarker)),
    sd = sd(biomarker, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(se = sd / sqrt(pmax(n, 1)))

p <- ggplot2::ggplot(df_sum, ggplot2::aes(x = visit_day, y = mean, color = trt, group = trt)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::labs(
    title = "Mean Biomarker Over Time by Treatment",
    x = "Visit day",
    y = "Mean biomarker"
  ) +
  THEME_CLINICAL()

fig_path <- file.path(PATHS$figures, "biomarker_mean_over_time.png")
save_png(fig_path, p)

eda_txt <- file.path(PATHS$results, "03_eda_summary.txt")
writeLines(c(
  paste0("EDA run at: ", Sys.time()),
  "",
  "Subjects:", as.character(dplyr::n_distinct(df$usubjid)),
  "Rows:", as.character(nrow(df)),
  "",
  "Missing biomarker overall:",
  paste0(sum(is.na(df$biomarker)), " / ", nrow(df), " (", round(mean(is.na(df$biomarker))*100, 1), "%)")
), eda_txt)

message("✅ Longitudinal EDA figure saved: ", fig_path)
message("✅ EDA summary saved: ", eda_txt)
