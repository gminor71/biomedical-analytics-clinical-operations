# src/02_data_cleaning.R
# Purpose: Create analysis-ready dataset for CT03 (longitudinal endpoint) aligned to SAP Week 0.

# ---- Inputs ----
in_rds  <- file.path(PATHS$data_proc, "01_imported_raw.rds")
out_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")

if (!file.exists(in_rds)) {
  stop("Missing import snapshot: ", in_rds, "\nRun: source('src/01_data_import.R')")
}

df <- readRDS(in_rds)

# ---- Expectations (based on shared synthetic trial) ----
required_cols <- c(
  "usubjid", "trt", "age", "sex", "bmi", "baseline_severity",
  "visit_day", "biomarker"
)
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
    pp  = NA
  )

# ---- Clean / standardize core analysis variables ----
df2 <- df2 %>%
  dplyr::mutate(
    trt = factor(trt, levels = c("Control", "Active")),
    sex = factor(sex, levels = c("F", "M")),
    age = as.numeric(age),
    bmi = as.numeric(bmi),
    baseline_severity = as.numeric(baseline_severity),
    visit_day = as.numeric(visit_day),
    biomarker = as.numeric(biomarker)
  )

# ---- Basic validity checks ----
qc_assert(all(df2$visit_day >= 0, na.rm = TRUE), "QC: visit_day must be >= 0.")
qc_assert(sum(df2$itt, na.rm = TRUE) > 0, "QC: No ITT rows found (trt missing for all).")

# ---- Analysis-ready dataset (ITT) ----
analysis_ready <- df2 %>%
  dplyr::filter(itt) %>%
  dplyr::select(
    usubjid, trt, age, sex, bmi, baseline_severity, visit_day, biomarker,
    dplyr::everything()
  )

# ---- Longitudinal key uniqueness check ----
key <- paste(analysis_ready$usubjid, analysis_ready$visit_day, sep = "||")
dup_key <- key[duplicated(key)]

if (length(dup_key) > 0) {
  stop(
    "Duplicate (usubjid, visit_day) rows found:\n",
    paste(unique(dup_key), collapse = ", ")
  )
}

# ---- Baseline presence check (helpful for interpretation) ----
has_bl <- analysis_ready %>%
  dplyr::group_by(usubjid) %>%
  dplyr::summarise(has_bl = any(visit_day == 0 & !is.na(biomarker)), .groups = "drop")

qc_assert(all(has_bl$has_bl), "QC: Some subjects missing baseline biomarker (visit_day==0).")

saveRDS(analysis_ready, out_rds)

# ---- QC outputs ----
qc_path <- file.path(PATHS$results, "QC_02_data_cleaning.txt")

n_subj <- dplyr::n_distinct(analysis_ready$usubjid)
n_rows <- nrow(analysis_ready)
n_by_trt <- table(analysis_ready$trt)

visits_by_subj <- analysis_ready %>%
  dplyr::group_by(usubjid) %>%
  dplyr::summarise(n_visits = dplyr::n(), .groups = "drop")

missing_bio_by_visit <- analysis_ready %>%
  dplyr::group_by(trt, visit_day) %>%
  dplyr::summarise(
    n = dplyr::n(),
    n_missing = sum(is.na(biomarker)),
    pct_missing = mean(is.na(biomarker)),
    .groups = "drop"
  )

qc_write(c(
  "QC: 02_data_cleaning.R (CT03 longitudinal)",
  paste0("Run at: ", Sys.time()),
  "",
  paste0("Subjects (ITT): ", n_subj),
  paste0("Rows (visits): ", n_rows),
  "",
  "N by treatment (subjects):",
  paste(names(n_by_trt), as.integer(n_by_trt), sep = ": "),
  "",
  "Visits per subject (summary):",
  paste(capture.output(print(summary(visits_by_subj$n_visits))), collapse = "\n"),
  "",
  "Missing biomarker by trt and visit_day:",
  paste(capture.output(print(missing_bio_by_visit)), collapse = "\n")
), qc_path)

message("✅ Saved analysis-ready dataset to: ", out_rds)
message("✅ QC report written: ", qc_path)
