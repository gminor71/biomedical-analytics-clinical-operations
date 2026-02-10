# src/02_data_cleaning.R
# Purpose: Create analysis-ready dataset for CT02 (time-to-event endpoint) aligned to SAP Week 0.

if (!exists("PATHS")) stop("PATHS not found. Run source('src/00_setup.R') first (or use 99_run_all.R).")

# ---- Inputs ----
in_rds  <- file.path(PATHS$data_proc, "01_imported_raw.rds")
out_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")

if (!file.exists(in_rds)) {
  stop("Missing import snapshot: ", in_rds, "\nRun: source('src/01_data_import.R')")
}

df <- readRDS(in_rds)

# ---- Expectations (based on shared synthetic trial) ----
required_cols <- c("usubjid", "trt", "age", "sex", "baseline_severity", "tte_days", "tte_event")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(
    "Required columns missing from imported data: ",
    paste(missing_cols, collapse = ", "),
    "\nCheck your raw file and/or 01_data_import.R name standardization."
  )
}

# ---- Define populations ----
df2 <- df %>%
  dplyr::mutate(
    itt = !is.na(trt),
    pp = NA
  )

# ---- Clean / standardize core analysis variables ----
# Standardize survival naming to time/status for downstream scripts
df2 <- df2 %>%
  dplyr::mutate(
    trt = factor(trt, levels = c("Control", "Active")),
    sex = factor(sex, levels = c("F", "M")),
    age = as.numeric(age),
    baseline_severity = as.numeric(baseline_severity),
    time = as.numeric(tte_days),
    status = as.integer(tte_event)
  )

# ---- Basic validity checks ----
# status must be 0/1
bad_status <- df2 %>%
  dplyr::filter(!is.na(status) & !(status %in% c(0L, 1L)))
if (nrow(bad_status) > 0) {
  stop("status contains values other than 0/1. Fix event coding before proceeding.")
}

# time must be >= 0 and non-missing for included rows
bad_time <- df2 %>%
  dplyr::filter(!is.na(time) & time < 0)
if (nrow(bad_time) > 0) {
  stop("time contains negative values. Fix time scale before proceeding.")
}

# must have at least some events overall
if (sum(df2$status[df2$itt], na.rm = TRUE) == 0) {
  stop("No events observed in ITT set (status==1). Cox model cannot be fit.")
}

# ---- Analysis-ready dataset (ITT) ----
analysis_ready <- df2 %>%
  dplyr::filter(itt) %>%
  dplyr::select(usubjid, trt, age, sex, baseline_severity, time, status, dplyr::everything())

# ---- ID uniqueness check ----
dup <- analysis_ready$usubjid[duplicated(analysis_ready$usubjid)]
if (length(dup) > 0) {
  stop(
    "Duplicate usubjid found in ITT dataset:\n",
    paste(unique(dup), collapse = ", ")
  )
}

saveRDS(analysis_ready, out_rds)

# ---- QC outputs ----
qc_path <- file.path(PATHS$results, "QC_02_data_cleaning.txt")

n_by_trt <- table(analysis_ready$trt)
events_by_trt <- with(analysis_ready, tapply(status, trt, sum))
qc_assert(all(events_by_trt > 0, na.rm = TRUE),
          "QC: One treatment group has 0 events. Cox model may be unstable or not estimable.")
event_rate_by_trt <- with(analysis_ready, tapply(status, trt, mean))
median_follow_by_trt <- with(analysis_ready, tapply(time, trt, median))
missingness <- sort(colSums(is.na(analysis_ready)), decreasing = TRUE)

qc_write(c(
  "QC: 02_data_cleaning.R (CT02 time-to-event)",
  paste0("Run at: ", Sys.time()),
  "",
  paste0("Rows (ITT): ", nrow(analysis_ready)),
  "",
  "Q by treatment:",
  qc_fmt_table(data.frame(trt = names(n_by_trt), n = as.integer(n_by_trt))),
  "",
  "Events by treatment (sanity only):",
  paste(names(events_by_trt), events_by_trt, sep = ": "),
  "",
  "Event rate by treatment (sanity only):",
  paste(names(event_rate_by_trt), round(event_rate_by_trt, 4), sep = ": "),
  "",
  "Median follow-up time by treatment (sanity only):",
  paste(names(median_follow_by_trt), round(median_follow_by_trt, 1), sep = ": "),
  "",
  "Missingness by column (descending):",
  paste(names(missingness), missingness, sep = ": ")
), qc_path)

message("✅ Saved analysis-ready dataset to: ", out_rds)
message("✅ QC report written: ", qc_path)
