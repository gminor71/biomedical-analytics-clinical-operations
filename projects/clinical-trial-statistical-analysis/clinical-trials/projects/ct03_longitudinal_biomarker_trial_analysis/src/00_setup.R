# src/00_setup.R
# Project setup: libraries, options, paths, reproducibility.

set.seed(12345)
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(gtsummary)
  library(broom)
  library(survival)
  library(survminer)
})

# Expect to run from the project root:
# clinical-trials/projects/<project_name>/
if (!dir.exists("src") || !dir.exists("data")) {
  stop(
    "Working directory is not the project root.\n",
    "Set working directory to: clinical-trials/projects/<project_name>/\n",
    "Then re-run: source('src/00_setup.R')"
  )
}

# ---- Shared helpers (hard dependencies) ----

shared_qc <- file.path("..", "..", "framework", "src", "qc_helpers.R")
shared_paths <- file.path("..", "..", "framework", "src", "utils_paths.R")
shared_visuals <- file.path("..", "..", "framework", "src", "utils_visuals.R")
shared_narrative <- file.path("..", "..", "framework", "src", "narrative_helpers.R")

for (f in c(shared_qc, shared_paths, shared_visuals, shared_narrative)) {
  if (!file.exists(f)) {
    stop(
      "Missing shared dependency: ", f, "\n",
      "Check that clinical-trials/shared/src is intact."
    )
  }
  source(f, local = FALSE)
}


# Build standardized project paths and ensure dirs exist
PATHS <- project_paths(find_project_root())
ensure_project_dirs(PATHS)

# ---- Project-specific narrative slots (per-study configuration) ----
slots_path <- file.path("src", "narrative_slots.R")
if (!file.exists(slots_path)) {
  stop(
    "Missing project narrative slots: ", slots_path, "\n",
    "Create it (project-specific) and define NARRATIVE_SLOTS."
  )
}
source(slots_path, local = FALSE)
qc_assert(exists("NARRATIVE_SLOTS"), "NARRATIVE_SLOTS not found after sourcing src/narrative_slots.R")

message("Project root: ", PATHS$root)


