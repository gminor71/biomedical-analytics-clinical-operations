# src/07_tables_figures.R
# Purpose: Build a results "packet" folder containing key outputs + an index
# Outputs:
# - results/packet_YYYYMMDD_HHMMSS/ (copied artifacts)
# - results/packet_.../INDEX.md
# - results/QC_07_results_packet.txt


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
    file.path(PATHS$figures, "km_by_trt.png"),
    file.path(PATHS$figures, "km_by_trt_risktable.png"),
    file.path(PATHS$figures, "ph_schoenfeld_trt.png")
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
    file.path(PATHS$tables, "interaction_hr_by_severity.csv"),
    file.path(PATHS$tables, "diagnostics_ph_test.csv"),
    file.path(PATHS$results, "diagnostics_influence_summary.csv")
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
  ok <- file.copy(paths, out_dir, overwrite = TRUE)
  qc_assert(all(ok), paste0("File copy failed in subdir: ", subdir))
  invisible(out_dir)
}

copy_into(artifacts$tables,  "tables")
copy_into(artifacts$figures, "figures")
copy_into(artifacts$models,  "models")
copy_into(artifacts$qc,      "qc")
copy_into(artifacts$misc,    "misc")

# ---- Build INDEX.md ----
index_path <- file.path(packet_dir, "INDEX.md")

index_lines <- c(
  "# CT02 Results Packet",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "## Tables",
  paste0("- [Table 1 – Baseline](tables/", basename(artifacts$tables[1]), ")"),
  paste0("- [Primary Cox model – HRs](tables/", basename(artifacts$tables[2]), ")"),
  paste0("- [Secondary Cox models](tables/", basename(artifacts$tables[3]), ")"),
  "",
  "## Figures",
  paste0("- [Kaplan–Meier curve by treatment](figures/", basename(artifacts$figures[1]), ")"),
  paste0("- [Kaplan–Meier risk table](figures/", basename(artifacts$figures[2]), ")"),
  paste0("- [Schoenfeld residuals: trt (PH check)](figures/", basename(artifacts$figures[3]), ")"),
  "",
  "## Key CSVs",
  paste0("- [Interaction HR by baseline severity](misc/", basename(artifacts$misc[1]), ")"),
  paste0("- [Proportional hazards test (cox.zph)](misc/", basename(artifacts$misc[2]), ")"),
  paste0("- [Influence summary (dfbeta)](misc/", basename(artifacts$misc[3]), ")"),
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
  "- Primary model aligns to SAP: Cox proportional hazards adjusted for age, sex, baseline severity.",
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

# Record latest packet directory for downstream steps (08)
latest_path <- file.path(PATHS$results, "LATEST_PACKET_DIR.txt")
writeLines(packet_dir, latest_path, useBytes = TRUE)

# =========================
# Refresh packet_reference_output to mirror latest packet (no narrative yet)
# =========================
ref_dir <- file.path(PATHS$results, "packet_reference_output")

if (dir.exists(ref_dir)) {
  unlink(
    list.files(ref_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
    recursive = TRUE,
    force = TRUE
  )
} else {
  dir.create(ref_dir, recursive = TRUE, showWarnings = FALSE)
}

copy_recursive <- function(from, to) {
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  items <- list.files(from, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  for (p in items) {
    dest <- file.path(to, basename(p))
    if (dir.exists(p)) {
      copy_recursive(p, dest)
    } else {
      ok <- file.copy(p, dest, overwrite = TRUE)
      if (!isTRUE(ok)) warning("Failed to copy into reference packet: ", p)
    }
  }
}

copy_recursive(packet_dir, ref_dir)
message("✅ Reference packet refreshed: ", ref_dir)