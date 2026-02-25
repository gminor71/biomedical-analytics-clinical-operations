# src/01_data_import.R
# Purpose: Read CT04 raw files and save data/processed/01_imported_raw.rds + QC

source("src/00_setup.R")

subj_path <- file.path(PATHS$data_raw, "ct04_subject_level.csv")
pk_path   <- file.path(PATHS$data_raw, "ct04_pk_long.csv")

if (!file.exists(subj_path)) stop("Missing: ct04_subject_level.csv (run 00_make_data.R)")
if (!file.exists(pk_path))   stop("Missing: ct04_pk_long.csv (run 00_make_data.R)")

subj_raw <- readr::read_csv(subj_path, show_col_types = FALSE)
pk_raw   <- readr::read_csv(pk_path, show_col_types = FALSE)

# Minimal standardization (keep consistent with other CTs)
subj_raw <- subj_raw |>
  dplyr::rename_with(~ stringr::str_replace_all(tolower(.x), "\\s+", "_")) |>
  dplyr::mutate(
    usubjid = as.character(usubjid),
    site    = as.character(site),
    trt     = stringr::str_to_title(as.character(trt)),
    sex     = as.character(sex)
  )

pk_raw <- pk_raw |>
  dplyr::rename_with(~ stringr::str_replace_all(tolower(.x), "\\s+", "_")) |>
  dplyr::mutate(
    usubjid = as.character(usubjid),
    dosegrp = as.character(dosegrp),
    dose    = as.numeric(dose),
    time    = as.numeric(time),
    conc    = as.numeric(conc)
  )

out_rds <- file.path(PATHS$data_proc, "01_imported_raw.rds")
saveRDS(list(subj = subj_raw, pk_long = pk_raw), out_rds)

message("✅ Saved: ", out_rds)
message("Imported subjects: ", nrow(subj_raw), " | PK rows: ", nrow(pk_raw))

# ---- QC ----
qc_path <- file.path(PATHS$results, "01_import_qc.txt")
writeLines(
  c(
    "01_import_qc: OK",
    paste0("ct04_subject_level.csv exists: ", file.exists(subj_path)),
    paste0("ct04_pk_long.csv exists: ", file.exists(pk_path)),
    paste0("01_imported_raw.rds exists: ", file.exists(out_rds))
  ),
  qc_path
)
