# src/02_data_cleaning.R
# Purpose: Create analysis-ready datasets and save data/processed/02_analysis_ready.rds + QC

source("src/00_setup.R")

DOSEGRP_LEVELS <- c("Placebo", "Low", "Medium", "High")

in_rds  <- file.path(PATHS$data_proc, "01_imported_raw.rds")
out_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")

if (!file.exists(in_rds)) stop("Missing: 01_imported_raw.rds (run 01_data_import.R)")

raw <- readRDS(in_rds)
subj <- raw$subj
pk   <- raw$pk_long

# Ensure dosegrp order and key columns exist
subj <- subj |>
  dplyr::mutate(
    dosegrp  = factor(as.character(dosegrp), levels = DOSEGRP_LEVELS),
    dose     = as.numeric(dose),
    pk_pop   = as.integer(pk_pop),
    resp_bin = as.integer(resp_bin),
    ae_any   = as.integer(ae_any),
    dlt      = as.integer(dlt)
  )

pk <- pk |>
  dplyr::mutate(
    dosegrp = factor(as.character(dosegrp), levels = DOSEGRP_LEVELS),
    dose    = as.numeric(dose),
    time    = as.numeric(time),
    conc    = as.numeric(conc)
  )

# Join subject covariates onto PK long (useful downstream)
pk <- pk |>
  dplyr::left_join(
    subj |>
      dplyr::select(usubjid, site, trt, age, sex, bmi, baseline_severity, dosegrp, dose, pk_pop),
    by = c("usubjid", "dosegrp", "dose")
  )

# ---- QC console ----
message("CT04 Cleaning QC")
message("Subjects: ", nrow(subj), " | Unique: ", dplyr::n_distinct(subj$usubjid))
message("PK rows: ", nrow(pk), " | Unique PK subjects: ", dplyr::n_distinct(pk$usubjid))
message("Dosegrp counts (subjects):")
print(subj |> dplyr::count(dosegrp))

# Save analysis-ready
saveRDS(list(subj = subj, pk_long = pk), out_rds)
message("✅ Saved: ", out_rds)

# ---- QC file ----
qc_path <- file.path(PATHS$results, "QC_02_data_cleaning.txt")
writeLines(
  c(
    "QC_02_data_cleaning: OK",
    paste0("02_analysis_ready.rds exists: ", file.exists(out_rds)),
    paste0("Subjects: ", nrow(subj), " (unique ", dplyr::n_distinct(subj$usubjid), ")"),
    paste0("PK rows: ", nrow(pk), " (unique ", dplyr::n_distinct(pk$usubjid), ")")
  ),
  qc_path
)
