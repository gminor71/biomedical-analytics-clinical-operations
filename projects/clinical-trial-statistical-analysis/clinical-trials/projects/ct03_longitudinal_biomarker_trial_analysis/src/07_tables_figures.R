# src/07_tables_figures.R
# Purpose: Build results "packets" (timestamped + reference) containing key outputs + an index
# Notes:
# - Timestamped packet is optional history.
# - packet_reference_output is the portfolio artifact and is refreshed every run.
# - Narrative is created in 08; we copy it here IF it already exists, and 08 can also copy into reference.

qc_assert(exists("PATHS"), "PATHS not found. Run 00_setup.R first (or run 99_run_all.R).")

timestamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
packet_dir <- file.path(PATHS$results, paste0("packet_", timestamp))
ref_dir    <- file.path(PATHS$results, "packet_reference_output")

dir.create(packet_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ref_dir,    recursive = TRUE, showWarnings = FALSE)

# ---- Define artifacts to package ----
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
    file.path(PATHS$tables,  "primary_emmeans_by_visit.csv")
  ),
  narrative = c(
    file.path(PATHS$results, "RESULTS_NARRATIVE.md"),
    file.path(PATHS$results, "RESULTS_NARRATIVE.txt")
  )
)

# Required artifacts for packet build (exclude narrative because it may not exist until 08 runs)
required <- unlist(artifacts[c("tables","figures","models","qc","misc")], use.names = FALSE)
missing_required <- required[!file.exists(required)]
qc_assert(length(missing_required) == 0,
          paste0("Results packet cannot be built; missing required artifacts:\n",
                 paste(" -", missing_required, collapse = "\n")))

# Narrative is optional here (08 may create it after 07 runs)
narrative_existing <- artifacts$narrative[file.exists(artifacts$narrative)]

copy_into_checked <- function(paths, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(paths, out_dir, overwrite = TRUE)
  if (!all(ok)) {
    stop(
      "File copy failed into: ", out_dir, "\nFailed copies:\n",
      paste(" -", paths[!ok], collapse = "\n")
    )
  }
  invisible(out_dir)
}

Sys.sleep(1)

# ---- Copy into timestamped packet ----
copy_into_checked(artifacts$tables,  file.path(packet_dir, "tables"))
copy_into_checked(artifacts$figures, file.path(packet_dir, "figures"))
copy_into_checked(artifacts$models,  file.path(packet_dir, "models"))
copy_into_checked(artifacts$qc,      file.path(packet_dir, "qc"))
copy_into_checked(artifacts$misc,    file.path(packet_dir, "misc"))
if (length(narrative_existing) > 0) {
  copy_into_checked(narrative_existing, file.path(packet_dir, "misc"))
}

# ---- Copy into reference packet (portfolio) ----
copy_into_checked(artifacts$tables,  file.path(ref_dir, "tables"))
copy_into_checked(artifacts$figures, file.path(ref_dir, "figures"))
copy_into_checked(artifacts$models,  file.path(ref_dir, "models"))
copy_into_checked(artifacts$qc,      file.path(ref_dir, "qc"))
copy_into_checked(artifacts$misc,    file.path(ref_dir, "misc"))
if (length(narrative_existing) > 0) {
  copy_into_checked(narrative_existing, file.path(ref_dir, "misc"))
}

# ---- INDEX (write in both places) ----
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
  if (file.exists(artifacts$narrative[1])) paste0("- [Results narrative (MD)](misc/", basename(artifacts$narrative[1]), ")") else NULL,
  if (file.exists(artifacts$narrative[2])) paste0("- [Results narrative (TXT)](misc/", basename(artifacts$narrative[2]), ")") else NULL,
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

writeLines(index_lines, file.path(packet_dir, "INDEX.md"), useBytes = TRUE)
writeLines(index_lines, file.path(ref_dir,    "INDEX.md"), useBytes = TRUE)

# ---- QC ----
qc_path <- file.path(PATHS$results, "QC_07_results_packet.txt")
qc_write(c(
  "QC: 07_tables_figures.R (Results packets)",
  paste0("Run at: ", Sys.time()),
  paste0("Timestamped packet: ", packet_dir),
  paste0("Reference packet:   ", ref_dir),
  "",
  "Artifacts copied (required):",
  paste0("  Tables:  ", length(artifacts$tables)),
  paste0("  Figures: ", length(artifacts$figures)),
  paste0("  Models:  ", length(artifacts$models)),
  paste0("  QC:      ", length(artifacts$qc)),
  paste0("  Misc:    ", length(artifacts$misc)),
  paste0("  Narrative copied now: ", length(narrative_existing)),
  "",
  paste0("INDEX (timestamped): ", file.path(packet_dir, "INDEX.md")),
  paste0("INDEX (reference):   ", file.path(ref_dir, "INDEX.md"))
), qc_path)

message("✅ Timestamped packet created: ", packet_dir)
message("✅ Reference packet refreshed: ", ref_dir)
message("✅ QC report written: ", qc_path)