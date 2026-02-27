# src/07_tables_figures.R
# Purpose: Produce ICH E3-style Tables/Figures for CT04 + QC

source("src/00_setup.R")

DOSEGRP_LEVELS <- c("Placebo", "Low", "Medium", "High")

# ---- Inputs ----
ready_path <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
pkp_path   <- file.path(PATHS$data_proc, "04_pk_params.rds")

dp_path     <- file.path(PATHS$results, "05_dose_proportionality_results.csv")
er_path     <- file.path(PATHS$results, "05_exposure_response_results.csv")
safety_path <- file.path(PATHS$results, "05_safety_by_dose_summary.csv")

stopifnot(file.exists(ready_path))
stopifnot(file.exists(pkp_path))
stopifnot(file.exists(dp_path))
stopifnot(file.exists(er_path))
stopifnot(file.exists(safety_path))

dat <- readRDS(ready_path)
subj <- dat$subj
pk   <- dat$pk_long

pkp <- readRDS(pkp_path)

dp_res <- readr::read_csv(dp_path, show_col_types = FALSE)
er_res <- readr::read_csv(er_path, show_col_types = FALSE)
safety_res <- readr::read_csv(safety_path, show_col_types = FALSE)

# Ensure factor order
subj <- subj |> dplyr::mutate(dosegrp = factor(as.character(dosegrp), levels = DOSEGRP_LEVELS))
pk   <- pk   |> dplyr::mutate(dosegrp = factor(as.character(dosegrp), levels = DOSEGRP_LEVELS))
pkp  <- pkp  |> dplyr::mutate(dosegrp = factor(as.character(dosegrp), levels = DOSEGRP_LEVELS))

# ---- Table 14.1.1 Baseline Characteristics by Dose Group (ITT) ----
t1411 <- subj |>
  dplyr::group_by(dosegrp) |>
  dplyr::summarize(
    N = dplyr::n(),
    Age_Mean = mean(age, na.rm = TRUE),
    Age_SD   = sd(age, na.rm = TRUE),
    BMI_Mean = mean(bmi, na.rm = TRUE),
    BMI_SD   = sd(bmi, na.rm = TRUE),
    BaselineSeverity_Mean = mean(baseline_severity, na.rm = TRUE),
    BaselineSeverity_SD   = sd(baseline_severity, na.rm = TRUE),
    Female_N = sum(sex %in% c("F","Female"), na.rm = TRUE),
    Female_Pct = 100 * mean(sex %in% c("F","Female"), na.rm = TRUE),
    .groups = "drop"
  )

out_t1411 <- file.path(PATHS$tables, "Table_14_1_1_Baseline.csv")
readr::write_csv(t1411, out_t1411)
message("✅ Wrote: ", out_t1411)

# ---- Table 14.2.1 PK Parameters by Dose (PK Population) ----
pk_summary <- pkp |>
  dplyr::group_by(dosegrp) |>
  dplyr::summarize(
    N = dplyr::n(),
    AUC_Mean    = mean(auc, na.rm = TRUE),
    AUC_SD      = sd(auc, na.rm = TRUE),
    AUC_Median  = median(auc, na.rm = TRUE),
    AUC_IQR     = IQR(auc, na.rm = TRUE),
    Cmax_Mean   = mean(cmax, na.rm = TRUE),
    Cmax_SD     = sd(cmax, na.rm = TRUE),
    Cmax_Median = median(cmax, na.rm = TRUE),
    Cmax_IQR    = IQR(cmax, na.rm = TRUE),
    .groups = "drop"
  )

out_t1421 <- file.path(PATHS$tables, "Table_14_2_1_PK_Params.csv")
readr::write_csv(pk_summary, out_t1421)
message("✅ Wrote: ", out_t1421)

# ---- Table 14.3.1 Dose Proportionality Model Summary ----
t1431 <- dp_res |>
  dplyr::mutate(
    Slope_Beta = slope_beta,
    CI_95 = sprintf("[%.3f, %.3f]", ci_low, ci_high)
  ) |>
  dplyr::select(metric, model, Slope_Beta, se, CI_95, p_value)

out_t1431 <- file.path(PATHS$tables, "Table_14_3_1_Dose_Prop.csv")
readr::write_csv(t1431, out_t1431)
message("✅ Wrote: ", out_t1431)

# ---- Table 14.4.1 Exposure–Response Model Results ----
t1441 <- er_res |>
  dplyr::mutate(
    OR = odds_ratio,
    OR_CI_95 = sprintf("[%.3f, %.3f]", or_ci_low, or_ci_high)
  ) |>
  dplyr::select(metric, model, OR, OR_CI_95, p_value)

out_t1441 <- file.path(PATHS$tables, "Table_14_4_1_Expo_Response.csv")
readr::write_csv(t1441, out_t1441)
message("✅ Wrote: ", out_t1441)

# ---- Table 14.5.1 Safety Summary by Dose Group ----
t1451 <- safety_res |>
  dplyr::transmute(
    dosegrp,
    N = n,
    AE_Any_N = ae_any_n,
    AE_Any_Pct = ae_any_pct,
    DLT_N = dlt_n,
    DLT_Pct = dlt_pct
  )

out_t1451 <- file.path(PATHS$tables, "Table_14_5_1_Safety.csv")
readr::write_csv(t1451, out_t1451)
message("✅ Wrote: ", out_t1451)

cat("\nTables present in PATHS$tables:\n")
print(list.files(PATHS$tables, full.names = FALSE))

# =========================
# Figures
# =========================

# ---- Figure 14.2.1 Mean Concentration–Time Profiles by Dose ----
pk_mean <- pk |>
  dplyr::group_by(dosegrp, time) |>
  dplyr::summarize(
    mean_conc = mean(conc, na.rm = TRUE),
    .groups = "drop"
  )

fig_1421 <- ggplot2::ggplot(pk_mean, ggplot2::aes(x = time, y = mean_conc, group = dosegrp)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::labs(
    title = "Figure 14.2.1 Mean Concentration–Time By Dose",
    x = "Time (hours)",
    y = "Mean Concentration"
  ) +
  ggplot2::theme_bw()

out_f1421 <- file.path(PATHS$figures, "Figure_14_2_1_Mean_Conc_Time.png")
ggplot2::ggsave(out_f1421, fig_1421, width = 9, height = 5.5, dpi = 180)
message("✅ Wrote: ", out_f1421)

# ---- Figure 14.2.2 Individual Concentration–Time Profiles by Dose (Spaghetti) ----
fig_1422 <- ggplot2::ggplot(pk, ggplot2::aes(x = time, y = conc, group = usubjid)) +
  ggplot2::geom_line(alpha = 0.25) +
  ggplot2::facet_wrap(~ dosegrp, scales = "free_y") +
  ggplot2::labs(
    title = "Figure 14.2.2 Individual Concentration–Time Profiles by Dose",
    x = "Time (hours)",
    y = "Concentration"
  ) +
  ggplot2::theme_bw()

out_f1422 <- file.path(PATHS$figures, "Figure_14_2_2_Indiv_Conc_Time.png")
ggplot2::ggsave(out_f1422, fig_1422, width = 10, height = 6, dpi = 180)
message("✅ Wrote: ", out_f1422)

# ---- Figure 14.4.1 Exposure–Response Relationship (AUC primary) ----
er_models_path <- file.path(PATHS$results, "05_models_exposure_response.rds")
stopifnot(file.exists(er_models_path))
mods <- readRDS(er_models_path)
fit_auc <- mods$fit_er_auc

auc_rng <- range(pkp$auc, na.rm = TRUE)
grid <- tibble::tibble(auc = seq(auc_rng[1], auc_rng[2], length.out = 200))
grid$pred_p <- predict(fit_auc, newdata = grid, type = "response")

fig_1441 <- ggplot2::ggplot(pkp, ggplot2::aes(x = auc, y = resp_bin)) +
  ggplot2::geom_jitter(height = 0.05, width = 0, alpha = 0.25) +
  ggplot2::geom_line(data = grid, ggplot2::aes(x = auc, y = pred_p), linewidth = 1) +
  ggplot2::labs(
    title = "Figure 14.4.1 Exposure–Response Relationship (AUC)",
    x = "Exposure (AUC)",
    y = "Response (binary) / Predicted Probability"
  ) +
  ggplot2::theme_bw()

out_f1441 <- file.path(PATHS$figures, "Figure_14_4_1_Expo_Response_AUC.png")
ggplot2::ggsave(out_f1441, fig_1441, width = 9, height = 5.5, dpi = 180)
message("✅ Wrote: ", out_f1441)

message("CT04 Tables/Figures complete.")

# =========================
# Results packet (CT04)
# =========================
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
packet_dir <- file.path(PATHS$results, paste0("packet_", timestamp))
dir.create(packet_dir, recursive = TRUE, showWarnings = FALSE)

artifacts <- list(
  tables = c(
    file.path(PATHS$tables, "Table_14_1_1_Baseline.csv"),
    file.path(PATHS$tables, "Table_14_2_1_PK_Params.csv"),
    file.path(PATHS$tables, "Table_14_3_1_Dose_Prop.csv"),
    file.path(PATHS$tables, "Table_14_4_1_Expo_Response.csv"),
    file.path(PATHS$tables, "Table_14_5_1_Safety.csv")
  ),
  figures = c(
    file.path(PATHS$figures, "Figure_14_2_1_Mean_Conc_Time.png"),
    file.path(PATHS$figures, "Figure_14_2_2_Indiv_Conc_Time.png"),
    file.path(PATHS$figures, "Figure_14_4_1_Expo_Response_AUC.png")
  ),
  models = c(
    file.path(PATHS$results, "05_models_dose_proportionality.rds"),
    file.path(PATHS$results, "05_models_exposure_response.rds"),
    file.path(PATHS$data_proc, "04_pk_params.rds")
  ),
  qc = c(
    file.path(PATHS$results, "QC_02_data_cleaning.txt"),
    file.path(PATHS$results, "QC_04_primary_analysis.txt"),
    file.path(PATHS$results, "QC_05_secondary_analyses.txt"),
    file.path(PATHS$results, "QC_06_diagnostics.txt")
  ),
  misc = c(
    file.path(PATHS$results, "05_dose_proportionality_results.csv"),
    file.path(PATHS$results, "05_exposure_response_results.csv"),
    file.path(PATHS$results, "05_safety_by_dose_summary.csv"),
    file.path(PATHS$results, "diagnostics", "06_diagnostics_summary.txt")
  )
)

all_expected <- unlist(artifacts, use.names = FALSE)
missing <- all_expected[!file.exists(all_expected)]

qc_assert(length(missing) == 0,
          paste0("Results packet cannot be built; missing artifacts:\n",
                 paste(" -", missing, collapse = "\n")))

copy_into <- function(paths, subdir) {
  out_dir <- file.path(packet_dir, subdir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(paths, out_dir, overwrite = TRUE)
  qc_assert(all(ok), paste0("File copy failed in subdir: ", subdir))
  invisible(out_dir)
}

copy_into(artifacts$tables,  "tables")
copy_into(artifacts$figures, "figures")
copy_into(artifacts$models,  "models")
copy_into(artifacts$qc,      "qc")
copy_into(artifacts$misc,    "misc")

# Index
index_path <- file.path(packet_dir, "INDEX.md")
index_lines <- c(
  "# CT04 Results Packet",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "## Tables (CSV)",
  paste0("- [Baseline characteristics](tables/", basename(artifacts$tables[1]), ")"),
  paste0("- [PK parameters by dose](tables/", basename(artifacts$tables[2]), ")"),
  paste0("- [Dose proportionality model](tables/", basename(artifacts$tables[3]), ")"),
  paste0("- [Exposure–response model](tables/", basename(artifacts$tables[4]), ")"),
  paste0("- [Safety summary](tables/", basename(artifacts$tables[5]), ")"),
  "",
  "## Figures (PNG)",
  paste0("- [Mean concentration–time](figures/", basename(artifacts$figures[1]), ")"),
  paste0("- [Individual profiles](figures/", basename(artifacts$figures[2]), ")"),
  paste0("- [Exposure–response curve](figures/", basename(artifacts$figures[3]), ")"),
  "",
  "## Models / Derived Data",
  paste0("- `models/", basename(artifacts$models[1]), "`"),
  paste0("- `models/", basename(artifacts$models[2]), "`"),
  paste0("- `models/", basename(artifacts$models[3]), "`"),
  "",
  "## Misc",
  paste0("- `misc/", basename(artifacts$misc[1]), "`"),
  paste0("- `misc/", basename(artifacts$misc[2]), "`"),
  paste0("- `misc/", basename(artifacts$misc[3]), "`"),
  paste0("- `misc/", basename(artifacts$misc[4]), "`"),
  "",
  "## QC Artifacts",
  paste0("- `qc/", basename(artifacts$qc[1]), "`"),
  paste0("- `qc/", basename(artifacts$qc[2]), "`"),
  paste0("- `qc/", basename(artifacts$qc[3]), "`"),
  paste0("- `qc/", basename(artifacts$qc[4]), "`"),
  "",
  "## Notes",
  "- Primary exposure metric: AUC (Cmax evaluated as sensitivity).",
  "- Dose proportionality assessed via log–log linear model excluding placebo.",
  "- Exposure–response assessed with logistic regression of response on exposure.",
  "- Safety summarized descriptively by AE and DLT by dose group."
)
writeLines(index_lines, index_path, useBytes = TRUE)

# QC report
qc_path <- file.path(PATHS$results, "QC_07_results_packet.txt")
qc_write(c(
  "QC: 07_tables_figures.R (Results packet)",
  paste0("Run at: ", Sys.time()),
  paste0("Packet directory: ", packet_dir),
  "",
  "Artifacts copied:",
  paste0("  Tables:  ", length(artifacts$tables)),
  paste0("  Figures: ", length(artifacts$figures)),
  paste0("  Models:  ", length(artifacts$models)),
  paste0("  QC:      ", length(artifacts$qc)),
  paste0("  Misc:    ", length(artifacts$misc)),
  "",
  paste0("INDEX: ", index_path)
), qc_path)

message("✅ Results packet created: ", packet_dir)
message("✅ INDEX written: ", index_path)
message("✅ QC report written: ", qc_path)

# Record latest packet directory for downstream steps (08)
latest_path <- file.path(PATHS$results, "LATEST_PACKET_DIR.txt")
writeLines(packet_dir, latest_path, useBytes = TRUE)