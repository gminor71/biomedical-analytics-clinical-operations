# src/99_run_all.R
# Purpose: Run full CT03 analysis pipeline and verify QC completion

scripts <- c(
  "src/01_data_import.R",
  "src/02_data_cleaning.R",
  "src/03_eda.R",
  "src/04_primary_analysis.R",
  "src/05_secondary_analyses.R",
  "src/06_diagnostics.R",
  "src/07_tables_figures.R",
  "src/08_results_narrative.R"
)

for (s in scripts) {
  message("▶ Running: ", s)
  tryCatch(
    source(s, echo = TRUE),
    error = function(e) {
      stop("Pipeline failed while running: ", s, "\n\n", conditionMessage(e))
    }
  )
}

qc_assert(exists("PATHS"), "QC: PATHS not found after pipeline run. Did 00_setup.R run successfully?")

qc_files_expected <- c(
  file.path(PATHS$results, "01_import_qc.txt"),
  file.path(PATHS$results, "QC_02_data_cleaning.txt"),
  file.path(PATHS$results, "QC_03_table1.txt"),
  file.path(PATHS$results, "QC_04_primary_analysis.txt"),
  file.path(PATHS$results, "QC_05_secondary_analyses.txt"),
  file.path(PATHS$results, "QC_06_diagnostics.txt"),
  file.path(PATHS$results, "QC_07_results_packet.txt"),
  file.path(PATHS$results, "QC_08_results_narrative.txt")
)

qc_exists <- file.exists(qc_files_expected)
if (!all(qc_exists)) {
  missing <- qc_files_expected[!qc_exists]
  stop(
    "FINAL QC CHECK FAILED.\n",
    "The following QC files are missing:\n",
    paste(" -", missing, collapse = "\n"),
    "\nPipeline halted."
  )
}

message("\n================ FINAL QC SUMMARY ================")
message("✔ Import QC completed (01_data_import)")
message("✔ Data cleaning QC completed (02_data_cleaning)")
message("✔ Table 1 QC completed (03_eda)")
message("✔ Primary analysis QC completed (04_primary_analysis)")
message("✔ Secondary analyses QC completed (05_secondary_analyses)")
message("✔ Diagnostics QC completed (06_diagnostics)")
message("✔ Results packet QC completed (07_tables_figures)")
message("✔ Narrative QC completed (08_results_narrative)")
message("✔ Pipeline completed successfully")
message("=================================================\n")
