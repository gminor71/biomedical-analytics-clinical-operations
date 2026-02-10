# rmd/tfl_shells/final/00_tfl_setup.R
# Purpose: Common setup for FINAL TFL render (no model re-fit)

suppressPackageStartupMessages({
  library(tidyverse)
  library(gt)
  library(gtsummary)
  library(broom)
})

# --- Project root detection ---
# Expected render working dir: clinical-trials/projects/<project>/
# But Quarto may render from the qmd folder, so we compute project root by walking up.
find_project_root_from_here <- function() {
  here <- normalizePath(".", winslash = "/", mustWork = TRUE)
  # Try up to 6 levels
  for (k in 0:6) {
    cand <- normalizePath(file.path(here, rep("..", k)), winslash = "/", mustWork = FALSE)
    if (dir.exists(file.path(cand, "src")) && dir.exists(file.path(cand, "data"))) return(cand)
  }
  stop("Could not find project root containing src/ and data/ from: ", here)
}

PROJ_ROOT <- find_project_root_from_here()

# Standard study folders (align with your project structure)
PATHS <- list(
  root    = PROJ_ROOT,
  results = file.path(PROJ_ROOT, "results"),
  tables  = file.path(PROJ_ROOT, "tables"),
  figures = file.path(PROJ_ROOT, "figures")
)

# Helper: require a file exists
req_file <- function(path) {
  if (!file.exists(path)) stop("Missing required file: ", path)
  path
}

# Helper: nice timestamp
stamp_time <- function(x = Sys.time()) format(x, "%Y-%m-%d %H:%M:%S")
