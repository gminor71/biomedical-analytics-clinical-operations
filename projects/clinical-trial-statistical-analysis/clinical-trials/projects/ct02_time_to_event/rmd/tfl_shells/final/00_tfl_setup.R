# rmd/tfl_shells/final/00_tfl_setup.R
# Purpose: Shared setup for TFL rendering (NO refitting)

# Run from project root when rendering; Quarto sets wd to qmd location sometimes,
# so we compute project root relative to this file.
here <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

# If we're inside rmd/tfl_shells/final, go up 3 levels to project root.
if (grepl("rmd/tfl_shells/final", here)) {
  setwd(normalizePath(file.path(here, "..", "..", ".."), winslash = "/"))
}

source("src/00_setup.R")  # defines PATHS + shared helpers

# Convenience paths for TFL layer
TFL <- list(
  tfl_root = file.path(PATHS$root, "rmd", "tfl_shells", "final"),
  tables   = PATHS$tables,
  figures  = PATHS$figures,
  results  = PATHS$results
)

# Small helper: fail fast if expected artifacts are missing
tfl_assert_files <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) stop("Missing required artifact(s):\n", paste(" -", missing, collapse="\n"))
}
