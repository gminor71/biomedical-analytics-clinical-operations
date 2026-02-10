# shared/src/narrative_helpers.R
# Purpose: Shared helper functions for generating CSR-style narrative text
#          from frozen pipeline outputs (no re-fitting).

# ---- Formatting helpers ----

fmt_p <- function(p) {
  if (is.null(p) || length(p) == 0 || is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

fmt_n <- function(n) {
  if (is.null(n) || length(n) == 0 || is.na(n)) return("NA")
  format(as.integer(n), big.mark = ",")
}

fmt_num <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("NA")
  sprintf(paste0("%.", digits, "f"), as.numeric(x))
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
trace_lines <- function(paths_named) {
  # paths_named: named character vector: c("Primary model"=".../primary_model.rds", ...)
  out <- character(0)
  for (nm in names(paths_named)) {
    p <- paths_named[[nm]]
    if (isTRUE(file.exists(p))) {
      out <- c(out, paste0("- ", nm, ": `", p, "`"))
    } else {
      out <- c(out, paste0("- ", nm, ": (missing) `", p, "`"))
    }
  }
  out
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE)
}

# ---- Slots support (project-specific text + term names) ----

validate_narrative_slots <- function(slots, required) {
  missing <- setdiff(required, names(slots))
  if (length(missing) > 0) {
    stop("NARRATIVE_SLOTS missing required fields: ", paste(missing, collapse = ", "))
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
