# src/01_data_import.R
# Purpose: Import raw trial data and save a stable processed snapshot (RDS).

if (!exists("PATHS")) stop("PATHS not found. Run source('src/00_setup.R') first (or use 99_run_all.R).")

# ---- USER INPUTS ----
# Put your raw file in: data/raw/
# Update filename + type here.
raw_filename <- "trial_longitudinal.csv"   # e.g., "trial_data.csv" or "trial_data.xlsx"
raw_sheet    <- 1                 # used only for Excel; can be sheet name or number

# Optional: if you want standardized snake_case column names
standardize_names <- TRUE

# ---- Helpers ----
to_snake <- function(x) {
  x <- gsub("\\s+", "_", x)
  x <- gsub("[^A-Za-z0-9_]", "", x)
  x <- gsub("_+", "_", x)
  tolower(x)
}

read_raw <- function(path, sheet = 1) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("csv")) {
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  } else if (ext %in% c("tsv", "txt")) {
    readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    # Excel support (install once if needed)
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required for Excel import. Install via renv::install('readxl')")
    }
    readxl::read_excel(path, sheet = sheet)
  } else {
    stop("Unsupported file type: ", ext, ". Use CSV/TSV/TXT/XLSX.")
  }
}

# ---- Import ----
raw_path <- file.path(PATHS$data_raw, raw_filename)

if (!file.exists(raw_path)) {
  stop(
    "Raw file not found: ", raw_path, "\n",
    "Place the file inside data/raw/ and confirm raw_filename is correct."
  )
}

df_raw <- read_raw(raw_path, sheet = raw_sheet) %>%
  as.data.frame()  # keeps behavior consistent for saving/logging

# ---- Standardize names (optional) ----
original_names <- names(df_raw)

if (standardize_names) {
  names(df_raw) <- to_snake(names(df_raw))
}

# ---- Quick import QC ----
qc <- list(
  imported_at   = as.character(Sys.time()),
  raw_file      = raw_filename,
  raw_path      = normalizePath(raw_path, winslash = "/", mustWork = FALSE),
  n_rows        = nrow(df_raw),
  n_cols        = ncol(df_raw),
  colnames_old  = original_names,
  colnames_new  = names(df_raw),
  missing_by_col = sort(colSums(is.na(df_raw)), decreasing = TRUE)
)

# ---- Save snapshot ----
# This is your immutable starting point for the project.
out_rds <- file.path(PATHS$data_proc, "01_imported_raw.rds")
saveRDS(df_raw, out_rds)

# Save QC log as RDS (easy to read back)
qc_rds <- file.path(PATHS$results, "01_import_qc.rds")
saveRDS(qc, qc_rds)

# Also save a human-readable text summary
qc_txt <- file.path(PATHS$results, "01_import_qc.txt")
writeLines(c(
  paste0("Imported at: ", qc$imported_at),
  paste0("Raw file: ", qc$raw_file),
  paste0("Raw path: ", qc$raw_path),
  paste0("Rows: ", qc$n_rows),
  paste0("Cols: ", qc$n_cols),
  "",
  "Top missingness by column (descending):",
  paste(names(qc$missing_by_col), qc$missing_by_col, sep = ": ")
), qc_txt)

message("✅ Imported raw data and saved snapshot to: ", out_rds)
message("✅ QC log saved to: ", qc_txt)
