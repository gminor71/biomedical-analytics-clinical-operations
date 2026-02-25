# src/00_setup.R

shared_qc <- file.path("..", "..", "framework", "src", "qc_helpers.R")
shared_paths <- file.path("..", "..", "framework", "src", "utils_paths.R")
shared_visuals <- file.path("..", "..", "framework", "src", "utils_visuals.R")
shared_narrative <- file.path("..", "..", "framework", "src", "narrative_helpers.R")

for (f in c(shared_qc, shared_paths, shared_visuals, shared_narrative)) {
  if (!file.exists(f)) {
    stop(
      "Missing shared dependency: ", f, "\n",
      "Check that framework/src is intact."
    )
  }
  source(f, local = FALSE)
}

PATHS <- project_paths(find_project_root())
ensure_project_dirs(PATHS)

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
