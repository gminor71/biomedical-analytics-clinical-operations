# shared/src/narrative_helpers.R
# Purpose: Shared helper functions for generating CSR-style narrative text
#          from frozen pipeline outputs (no re-fitting).

# ---- Formatting helpers ----

fmt_p <- function(p, digits = 3) {
  if (is.null(p)) return(NA_character_)
  if (length(p) == 0) return(character(0))
  p <- suppressWarnings(as.numeric(p))
  out <- rep(NA_character_, length(p))
  ok <- !is.na(p)
  out[ok] <- ifelse(p[ok] < 10^(-digits),
                    paste0("<", formatC(10^(-digits), format="f", digits=digits)),
                    formatC(p[ok], format="f", digits=digits))
  out
}

fmt_n <- function(n) {
  if (is.null(n) || length(n) == 0 || is.na(n)) return("NA")
  format(as.integer(n), big.mark = ",")
}

fmt_num <- function(x, digits = 1) {
  # Vector-safe numeric formatter; returns character vector
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0) return(character(0))
  
  x_num <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x_num))
  
  ok <- !is.na(x_num)
  out[ok] <- formatC(x_num[ok], format = "f", digits = digits)
  
  out
}

fmt_ci <- function(est, lo, hi, digits = 2, label = NULL) {
  if (any(is.na(c(est, lo, hi)))) return("NA")
  core <- sprintf(
    paste0("%.", digits, "f (95%% CI %.", digits, "f–%.", digits, "f)"),
    as.numeric(est), as.numeric(lo), as.numeric(hi)
  )
  if (!is.null(label) && nzchar(label)) paste0(label, " ", core) else core
}

fmt_range <- function(lo, hi, digits = 1) {
  if (any(is.na(c(lo, hi)))) return("NA")
  sprintf(paste0("%.", digits, "f–%.", digits, "f"), as.numeric(lo), as.numeric(hi))
}

stamp_time <- function(x = Sys.time()) {
  format(x, "%Y-%m-%d %H:%M:%S")
}

# ---- Industry-style phrasing helpers ----
# These return single sentences you can drop into the narrative.

sent_primary_or <- function(or, lo, hi, p, covariates = "age, sex, and baseline severity") {
  paste0(
    "In the primary adjusted logistic regression model (adjusted for ", covariates, "), ",
    "Active treatment was associated with an adjusted odds ratio of ",
    fmt_ci(or, lo, hi, digits = 2),
    " compared with Control (p=", fmt_p(p), ")."
  )
}

sent_primary_hr <- function(hr, lo, hi, p, covariates = "age, sex, and baseline severity") {
  paste0(
    "In the primary adjusted Cox proportional hazards model (adjusted for ", covariates, "), ",
    "Active treatment was associated with a hazard ratio of ",
    fmt_ci(hr, lo, hi, digits = 2),
    " compared with Control (p=", fmt_p(p), ")."
  )
}

sent_primary_lme_slope <- function(beta, se, p, unit = "per 30 days") {
  paste0(
    "In the primary linear mixed-effects model, the prespecified treatment-by-time interaction ",
    "(difference in slope ", unit, ") was ",
    fmt_num(beta, 4), " (SE=", fmt_num(se, 4), "; p=", fmt_p(p), ")."
  )
}

sent_baseline_table1 <- function() {
  paste0(
    "Baseline characteristics were summarized descriptively by treatment group (Table 1). ",
    "No hypothesis testing was performed for baseline comparisons."
  )
}

sent_diagnostics_ph <- function(p_global = NA) {
  if (is.na(p_global)) {
    "The proportional hazards assumption was assessed using Schoenfeld residuals."
  } else {
    paste0(
      "The proportional hazards assumption was assessed using Schoenfeld residuals ",
      "(global p=", fmt_p(p_global), ")."
    )
  }
}

sent_conclusion_generic <- function() {
  paste0(
    "Overall, the primary model and prespecified supportive analyses provide the basis for the study interpretation. ",
    "Results should be interpreted in the context of model assumptions and the exploratory nature of interaction assessments."
  )
}

# ---- Traceability helpers ----
trace_lines <- function(named_paths) {
  # named_paths: named character vector, e.g. c("Label" = "path/to/file", ...)
  if (is.null(named_paths) || length(named_paths) == 0) return(character(0))
  
  labs <- names(named_paths)
  paths <- unname(as.character(named_paths))
  if (is.null(labs) || any(labs == "")) labs <- rep("Output", length(paths))
  
  exists_flag <- file.exists(paths)
  
  out <- character(length(paths))
  for (i in seq_along(paths)) {
    status <- if (isTRUE(exists_flag[i])) "OK" else "MISSING"
    out[i] <- paste0("- **", labs[i], "**: `", paths[i], "` (", status, ")")
  }
  out
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE)
}

# ---- Slots support (project-specific text + term names) ----
validate_narrative_slots <- function(slots, required = character()) {
  # slots must be a list
  if (is.null(slots) || !is.list(slots)) {
    stop("NARRATIVE_SLOTS must be a non-null list.")
  }
  
  # If no required keys, nothing to validate
  if (length(required) == 0) return(invisible(TRUE))
  
  have <- names(slots)
  missing <- setdiff(required, have)
  
  if (length(missing) > 0) {
    stop(
      "NARRATIVE_SLOTS is missing required keys: ",
      paste(missing, collapse = ", ")
    )
  }
  
  invisible(TRUE)
}

# Slots-aware sentences (use contrast_label + covariates_text)
sent_primary_or_slots <- function(or, lo, hi, p, slots) {
  covs <- if (!is.null(slots$covariates_text)) slots$covariates_text else "age, sex, and baseline severity"
  contrast <- if (!is.null(slots$contrast_label)) slots$contrast_label else "Active vs Control"
  paste0(
    "In the primary adjusted logistic regression model (adjusted for ", covs, "), ",
    contrast, " was associated with an adjusted odds ratio of ",
    fmt_ci(or, lo, hi, digits = 2),
    " (p=", fmt_p(p), ")."
  )
}

sent_primary_hr_slots <- function(hr, lo, hi, p, slots) {
  covs <- if (!is.null(slots$covariates_text)) slots$covariates_text else "age, sex, and baseline severity"
  contrast <- if (!is.null(slots$contrast_label)) slots$contrast_label else "Active vs Control"
  paste0(
    "In the primary adjusted Cox proportional hazards model (adjusted for ", covs, "), ",
    contrast, " was associated with a hazard ratio of ",
    fmt_ci(hr, lo, hi, digits = 2),
    " (p=", fmt_p(p), ")."
  )
}

sent_primary_lme_slope_slots <- function(beta, se, p, slots) {
  unit <- if (!is.null(slots$slope_unit)) slots$slope_unit else "per unit time"
  paste0(
    "In the primary linear mixed-effects model, the prespecified treatment-by-time interaction ",
    "(difference in slope ", unit, ") was ",
    fmt_num(beta, 4), " (SE=", fmt_num(se, 4), "; p=", fmt_p(p), ")."
  )
}

sent_diagnostics_lme_slots <- function(slots) {
  if (!is.null(slots$diag_sentence_lme) && nzchar(slots$diag_sentence_lme)) {
    return(slots$diag_sentence_lme)
  }
  "Model convergence and fit diagnostics were reviewed; estimation used maximum likelihood under a MAR assumption."
}
