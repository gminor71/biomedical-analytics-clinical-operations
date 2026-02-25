# src/02_data_cleaning.R
# Purpose: Create analysis-ready dataset for CT01 (binary endpoint) aligned to SAP Week 0.

source("src/00_setup.R")


# ---- Inputs ----
in_rds  <- file.path(PATHS$data_proc, "01_imported_raw.rds")
out_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")

if (!file.exists(in_rds)) {
  stop("Missing import snapshot: ", in_rds, "\nRun: source('src/01_data_import.R')")
}

df <- readRDS(in_rds)

# ---- Expectations (based on synthetic shared trial) ----
required_cols <- c("usubjid", "trt", "age", "sex", "baseline_severity", "event_bin")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(
    "Required columns missing from imported data: ",
    paste(missing_cols, collapse = ", "),
    "\nCheck your raw file and/or 01_data_import.R name standardization."
  )
}

# ---- Define populations ----
# ITT: all randomized subjects with assigned treatment
# (In real trials you might exclude screen failures; here we keep everyone with non-missing trt)
df2 <- df %>%
  dplyr::mutate(
    itt = !is.na(trt),
    # Placeholder for later: per-protocol (PP) definition could be added here
    pp = NA
  )

# ---- Clean / standardize core analysis variables ----
df2 <- df2 %>%
  dplyr::mutate(
    trt = factor(trt, levels = c("Control", "Active")),
    sex = factor(sex),
    age = as.numeric(age),
    baseline_severity = as.numeric(baseline_severity),
    event_bin = as.integer(event_bin)
  )

# ---- Basic validity checks ----
# Endpoint should be 0/1
bad_event <- df2 %>%
  dplyr::filter(!is.na(event_bin) & !(event_bin %in% c(0L, 1L)))
if (nrow(bad_event) > 0) {
  stop("event_bin contains values other than 0/1. Fix endpoint coding before proceeding.")
}

# ---- Analysis-ready dataset (ITT) ----
analysis_ready <- df2 %>%
  dplyr::filter(itt) %>%
  dplyr::select(usubjid, trt, age, sex, baseline_severity, event_bin, dplyr::everything())

# ---- ID uniqueness check ----
dup <- analysis_ready$usubjid[duplicated(analysis_ready$usubjid)]
if (length(dup) > 0) {
  stop(
    "Duplicate usubjid found in ITT dataset:\n",
    paste(unique(dup), collapse = ", ")
  )
}

saveRDS(analysis_ready, out_rds)

qc <- qc_dataset_cleaning(
  analysis_ready,
  required_cols = c("usubjid", "trt", "age", "sex", "baseline_severity", "event_bin"),
  trt_col = "trt",
  endpoint_col = "event_bin"
)

qc_path <- file.path(PATHS$results, "QC_02_data_cleaning.txt")

qc_write(c(
  paste0("QC: 02_data_cleaning.R"),
  paste0("Run at: ", Sys.time()),
  "",
  paste0("Rows: ", qc$n),
  "",
  "N by treatment:",
  qc_fmt_table(qc$n_by_trt),
  "",
  "Event rate by treatment (sanity only):",
  paste(names(qc$event_rate_by_trt), round(qc$event_rate_by_trt, 4), sep = ": "),
  "",
  "Missingness by column (descending):",
  paste(names(qc$miss), qc$miss, sep = ": ")
), qc_path)

message("✅ QC report written: ", qc_path)

# ---- Write a small QC summary to results/ ----
qc_txt <- file.path(PATHS$results, "02_cleaning_qc.txt")

event_rate_by_trt <- analysis_ready %>%
  dplyr::group_by(trt) %>%
  dplyr::summarise(
    n = dplyr::n(),
    event_rate = mean(event_bin, na.rm = TRUE),
    .groups = "drop"
  )

missingness <- sort(colSums(is.na(analysis_ready)), decreasing = TRUE)

writeLines(c(
  paste0("Cleaning run at: ", as.character(Sys.time())),
  paste0("Rows (ITT): ", nrow(analysis_ready)),
  paste0("Cols: ", ncol(analysis_ready)),
  "",
  "Event rate by treatment:",
  paste0(capture.output(print(event_rate_by_trt)), collapse = "\n"),
  "",
  "Missingness by column (descending):",
  paste(names(missingness), missingness, sep = ": ")
), qc_txt)

message("✅ Saved analysis-ready dataset to: ", out_rds)
message("✅ QC summary saved to: ", qc_txt)

