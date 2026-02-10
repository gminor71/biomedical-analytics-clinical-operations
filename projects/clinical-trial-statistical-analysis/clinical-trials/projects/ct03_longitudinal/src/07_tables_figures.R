# src/07_tables_figures.R
# Purpose: Build a results "packet" folder containing key outputs + an index

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
packet_dir <- file.path(PATHS$results, paste0("packet_", timestamp))
dir.create(packet_dir, recursive = TRUE, showWarnings = FALSE)

artifacts <- list(
  tables = c(
    file.path(PATHS$tables, "table1_baseline.html"),
    file.path(PATHS$tables, "primary_model.html"),
    file.path(PATHS$tables, "secondary_models.html")
  ),
  figures = c(
    file.path(PATHS$figures, "biomarker_mean_over_time.png"),
    file.path(PATHS$figures, "diag_resid_vs_fitted.png"),
    file.path(PATHS$figures, "diag_resid_qq.png"),
    file.path(PATHS$figures, "diag_ranef_intercepts.png")
  ),
  models = c(
    file.path(PATHS$results, "primary_model.rds"),
    file.path(PATHS$results, "secondary_models.rds")
  ),
  qc = c(
    file.path(PATHS$results, "QC_02_data_cleaning.txt"),
    file.path(PATHS$results, "QC_03_table1.txt"),
    file.path(PATHS$results, "QC_04_primary_analysis.txt"),
    file.path(PATHS$results, "QC_05_secondary_analyses.txt"),
    file.path(PATHS$results, "QC_06_diagnostics.txt")
  ),
  misc = c(
    file.path(PATHS$results, "diagnostics_residuals_summary.csv"),
    file.path(PATHS$tables, "primary_emmeans_by_visit.csv")
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
  file.copy(paths, out_dir, overwrite = TRUE)
  invisible(out_dir)
}

copy_into(artifacts$tables,  "tables")
copy_into(artifacts$figures, "figures")
copy_into(artifacts$models,  "models")
copy_into(artifacts$qc,      "qc")
copy_into(artifacts$misc,    "misc")

index_path <- file.path(packet_dir, "INDEX.md")

index_lines <- c(
  "# CT03 Results Packet",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "## Tables",
  paste0("- [Table 1 – Baseline](tables/", basename(artifacts$tables[1]), ")"),
  paste0("- [Primary mixed model](tables/", basename(artifacts$tables[2]), ")"),
  paste0("- [Secondary models](tables/", basename(artifacts$tables[3]), ")"),
  "",
  "## Figures",
  paste0("- [Mean biomarker over time](figures/", basename(artifacts$figures[1]), ")"),
  paste0("- [Residuals vs fitted](figures/", basename(artifacts$figures[2]), ")"),
  paste0("- [Q-Q residuals](figures/", basename(artifacts$figures[3]), ")"),
  paste0("- [Random intercepts](figures/", basename(artifacts$figures[4]), ")"),
  "",
  "## Misc CSVs",
  paste0("- [Residuals table](misc/", basename(artifacts$misc[1]), ")"),
  paste0("- [Estimated means by visit](misc/", basename(artifacts$misc[2]), ")"),
  "",
  "## QC Artifacts",
  paste0("- `qc/", basename(artifacts$qc[1]), "`"),
  paste0("- `qc/", basename(artifacts$qc[2]), "`"),
  paste0("- `qc/", basename(artifacts$qc[3]), "`"),
  paste0("- `qc/", basename(artifacts$qc[4]), "`"),
  paste0("- `qc/", basename(artifacts$qc[5]), "`"),
  "",
  "## Notes",
  "- Table 1 is baseline-only (one row per subject).",
  "- Primary model: mixed model with trt-by-time interaction (time scaled per 30 days).",
  "- Secondary models include sensitivity and exploratory random slopes / effect modification."
)

writeLines(index_lines, index_path, useBytes = TRUE)

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
