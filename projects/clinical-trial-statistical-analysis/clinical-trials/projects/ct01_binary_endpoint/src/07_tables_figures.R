# src/07_tables_figures.R
# Purpose: Build a results "packet" folder containing key outputs + an index
# Outputs:
# - results/packet_YYYYMMDD_HHMMSS/ (copied artifacts)
# - results/packet_.../INDEX.md
# - results/QC_07_results_packet.txt
# - optional results/packet_...zip

source("src/00_setup.R")


timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
packet_dir <- file.path(PATHS$results, paste0("packet_", timestamp))
dir.create(packet_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Define expected artifacts ----
artifacts <- list(
  tables = c(
    file.path(PATHS$tables, "table1_baseline.html"),
    file.path(PATHS$tables, "primary_model.html"),
    file.path(PATHS$tables, "secondary_models.html")
  ),
  figures = c(
    file.path(PATHS$figures, "diag_residuals_vs_fitted.png"),
    file.path(PATHS$figures, "diag_std_residuals_qq.png"),
    file.path(PATHS$figures, "diag_cooks_distance.png"),
    file.path(PATHS$figures, "diag_leverage.png")
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
    file.path(PATHS$tables, "interaction_or_by_severity.csv"),
    file.path(PATHS$results, "diagnostics_influence_summary.csv"),
    file.path(PATHS$tables, "diagnostics_vif.csv")
  )
)

all_expected <- unlist(artifacts, use.names = FALSE)
missing <- all_expected[!file.exists(all_expected)]

qc_assert(length(missing) == 0,
          paste0(
            "Results packet cannot be built; missing artifacts:\n",
            paste(" -", missing, collapse = "\n")
          ))

# ---- Copy artifacts into packet folder (keep simple structure) ----
copy_into <- function(paths, subdir) {
  out_dir <- file.path(packet_dir, subdir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(paths, out_dir, overwrite = TRUE)
  invisible(out_dir)
}

dir_tables  <- copy_into(artifacts$tables,  "tables")
dir_figures <- copy_into(artifacts$figures, "figures")
dir_models  <- copy_into(artifacts$models,  "models")
dir_qc      <- copy_into(artifacts$qc,      "qc")
dir_misc    <- copy_into(artifacts$misc,    "misc")

# ---- Build INDEX.md ----
#rel <- function(path) {
  # Convert absolute packet paths to relative links inside the packet
 # sub("^.*packet_[0-9]{8}_[0-9]{6}/", "", path)
#} ## placeholder for when add HTML/PDF

index_path <- file.path(packet_dir, "INDEX.md")

index_lines <- c(
  "# CT01 Results Packet",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "## Tables",
  paste0("- [Table 1 – Baseline](tables/", basename(artifacts$tables[1]), ")"),
  paste0("- [Primary model – ORs](tables/", basename(artifacts$tables[2]), ")"),
  paste0("- [Secondary models](tables/", basename(artifacts$tables[3]), ")"),
  "",
  "## Figures (Diagnostics)",
  paste0("- [Residuals vs fitted](figures/", basename(artifacts$figures[1]), ")"),
  paste0("- [Q-Q standardized deviance residuals](figures/", basename(artifacts$figures[2]), ")"),
  paste0("- [Cook's distance](figures/", basename(artifacts$figures[3]), ")"),
  paste0("- [Leverage](figures/", basename(artifacts$figures[4]), ")"),
  "",
  "## Key CSVs",
  paste0("- [Interaction OR by baseline severity](misc/", basename(artifacts$misc[1]), ")"),
  paste0("- [Influence summary](misc/", basename(artifacts$misc[2]), ")"),
  paste0("- [VIF / GVIF](misc/", basename(artifacts$misc[3]), ")"),
  "",
  "## Model Objects (RDS)",
  paste0("- `models/", basename(artifacts$models[1]), "`"),
  paste0("- `models/", basename(artifacts$models[2]), "`"),
  "",
  "## QC Artifacts",
  paste0("- `qc/", basename(artifacts$qc[1]), "`"),
  paste0("- `qc/", basename(artifacts$qc[2]), "`"),
  paste0("- `qc/", basename(artifacts$qc[3]), "`"),
  paste0("- `qc/", basename(artifacts$qc[4]), "`"),
  paste0("- `qc/", basename(artifacts$qc[5]), "`"),
  "",
  "## Notes",
  "- Table 1 is descriptive only (no p-values).",
  "- Primary model aligns to SAP: logistic regression adjusted for age, sex, baseline severity.",
  "- Secondary includes sensitivity (drop baseline severity) and exploratory interaction."
)

writeLines(index_lines, index_path, useBytes = TRUE)

# ---- QC report ----
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

