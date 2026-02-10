# shared/src/utils_paths.R
# Robust path helpers for this repo structure:
# clinical-trials/
#   ├─ shared/src/utils_paths.R
#   └─ projects/<project_name>/{data,src,rmd,figures,tables,results,...}

# Find the project root by walking up until we see folders that define a project
find_project_root <- function(start = getwd(), markers = c("src", "data", "rmd")) {
  dir <- normalizePath(start, winslash = "/", mustWork = FALSE)
  
  for (.i in 1:20) {
    ok <- all(file.exists(file.path(dir, markers)))
    if (ok) return(dir)
    
    parent <- normalizePath(file.path(dir, ".."), winslash = "/", mustWork = FALSE)
    if (identical(parent, dir)) break
    dir <- parent
  }
  
  stop(
    "Could not locate project root. Run from inside a project folder (projects/<name>/...) ",
    "or set working directory to the project root."
  )
}

# Build standard project paths (raw/processed enforced)
project_paths <- function(project_root = find_project_root()) {
  list(
    root      = project_root,
    data_raw  = file.path(project_root, "data", "raw"),
    data_proc = file.path(project_root, "data", "processed"),
    figures   = file.path(project_root, "figures"),
    tables    = file.path(project_root, "tables"),
    results   = file.path(project_root, "results"),
    notebooks = file.path(project_root, "notebooks"),
    rmd       = file.path(project_root, "rmd"),
    src       = file.path(project_root, "src")
  )
}

# Create missing directories (safe to re-run)
ensure_project_dirs <- function(paths) {
  dirs <- unname(unlist(paths, use.names = FALSE))
  dirs <- dirs[grepl("(data|figures|tables|results|notebooks|rmd|src)", dirs)]
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  invisible(paths)
}
