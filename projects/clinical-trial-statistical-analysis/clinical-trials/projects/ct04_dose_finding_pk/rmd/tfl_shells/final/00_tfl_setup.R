# rmd/tfl_shells/final/00_tfl_setup.R
# Purpose: Common setup for FINAL TFL render (no model re-fit)

suppressPackageStartupMessages({
  library(tidyverse)
  library(gt)
  library(gtsummary)
  library(broom)
})

# --- Project root detection ---
# Expected project structure:
# clinical-trials/projects/<project>/
# Quarto may render from inside rmd/tfl_shells/final/, so we walk up.
find_project_root_from_here <- function(max_up = 8) {
  here <- normalizePath(".", winslash = "/", mustWork = TRUE)
  
  for (k in 0:max_up) {
    cand <- normalizePath(file.path(here, rep("..", k)), winslash = "/", mustWork = FALSE)
    if (dir.exists(file.path(cand, "src")) && dir.exists(file.path(cand, "data"))) {
      return(cand)
    }
  }
  
  stop("Could not find project root containing src/ and data/ from: ", here)
}

PROJ_ROOT <- find_project_root_from_here()

# Standard study folders (align with your project structure)
PATHS <- list(
  root      = PROJ_ROOT,
  results   = file.path(PROJ_ROOT, "results"),
  tables    = file.path(PROJ_ROOT, "tables"),
  figures   = file.path(PROJ_ROOT, "figures"),
  data_proc = file.path(PROJ_ROOT, "data", "processed"),
  rmd_final = file.path(PROJ_ROOT, "rmd", "tfl_shells", "final")
)

# Ensure output folders exist (harmless if already present)
dir.create(PATHS$tables,  recursive = TRUE, showWarnings = FALSE)
dir.create(PATHS$figures, recursive = TRUE, showWarnings = FALSE)

# Helper: require a file exists
req_file <- function(path) {
  if (!file.exists(path)) stop("Missing required file: ", path)
  path
}

# Helper: safe CSV read (returns tibble or stops)
safe_read_csv <- function(path, ...) {
  req_file(path)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE, ...)
}

# Helper: safe RDS read
safe_read_rds <- function(path) {
  req_file(path)
  readRDS(path)
}

# Helper: nice timestamp
stamp_time <- function(x = Sys.time()) format(x, "%Y-%m-%d %H:%M:%S")

# ---- Optional formatting helpers (match your narrative style) ----
fmt_num <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_pct <- function(x, digits = 1) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
  sprintf(paste0("%.", digits, "f%%"), x)
}

fmt_ci <- function(est, lo, hi, digits = 3) {
  paste0(fmt_num(est, digits), " [", fmt_num(lo, digits), ", ", fmt_num(hi, digits), "]")
}