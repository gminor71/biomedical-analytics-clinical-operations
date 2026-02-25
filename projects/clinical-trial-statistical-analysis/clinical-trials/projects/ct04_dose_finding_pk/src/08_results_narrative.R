# src/08_results_narrative.R
# Purpose: Convert existing CT04 pipeline outputs into narrative text WITHOUT re-running analyses.
# Outputs:
# - results/RESULTS_NARRATIVE.md
# - results/RESULTS_NARRATIVE.txt
# - results/QC_08_results_narrative.txt


if (!exists("PATHS")) stop("PATHS not found. Run 00_setup.R first (or run 99_run_all.R).")

# ---- Narrative “slots” (project-specific) ----
source("src/narrative_slots.R", local = FALSE)

qc_assert(exists("NARRATIVE_SLOTS"), "NARRATIVE_SLOTS not found. Did you create src/narrative_slots.R?")

# Optional but strongly recommended: validate required keys (CT04-specific)
validate_narrative_slots(
  NARRATIVE_SLOTS,
  required = c("study_id", "study_title", "dose_groups_text")
)

project_name <- paste0(NARRATIVE_SLOTS$study_id, " - ", NARRATIVE_SLOTS$study_title)
run_time <- Sys.time()

# ---- Locate frozen artifacts (prefer RDS/CSV) ----
pk_params_rds <- file.path(PATHS$data_proc, "04_pk_params.rds")

dp_csv     <- file.path(PATHS$results, "05_dose_proportionality_results.csv")
er_csv     <- file.path(PATHS$results, "05_exposure_response_results.csv")
safety_csv <- file.path(PATHS$results, "05_safety_by_dose_summary.csv")

qc02 <- file.path(PATHS$results, "QC_02_data_cleaning.txt")
qc04 <- file.path(PATHS$results, "QC_04_primary_analysis.txt")
qc05 <- file.path(PATHS$results, "QC_05_secondary_analyses.txt")
qc06 <- file.path(PATHS$results, "QC_06_diagnostics.txt")

# ---- QC: required files exist ----
qc_assert(file.exists(pk_params_rds), paste0("Missing: ", pk_params_rds))
qc_assert(file.exists(dp_csv),        paste0("Missing: ", dp_csv))
qc_assert(file.exists(er_csv),        paste0("Missing: ", er_csv))
qc_assert(file.exists(safety_csv),    paste0("Missing: ", safety_csv))
qc_assert(file.exists(qc02) && file.exists(qc04) && file.exists(qc05),
          "Missing QC inputs required for narrative (QC_02/QC_04/QC_05).")

# ---- Read frozen objects ----
pkp <- readRDS(pk_params_rds)
dp  <- safe_read_csv(dp_csv)
er  <- safe_read_csv(er_csv)
sf  <- safe_read_csv(safety_csv)

qc_assert(!is.null(dp) && "metric" %in% names(dp), "Dose proportionality CSV missing expected columns.")
qc_assert(!is.null(er) && "metric" %in% names(er), "Exposure-response CSV missing expected columns.")
qc_assert(!is.null(sf) && "dosegrp" %in% names(sf), "Safety CSV missing expected columns.")

# ---- Extract key results (AUC primary) ----
pick_first_metric <- function(df, metric_value) {
  if (is.null(df) || !("metric" %in% names(df))) return(NULL)
  m <- as.character(df$metric)
  idx <- which(m == metric_value)
  if (length(idx) < 1) return(NULL)
  df[idx[1], , drop = FALSE]
}

dp_auc <- pick_first_metric(dp, "AUC")
qc_assert(!is.null(dp_auc), "Dose proportionality results missing metric == 'AUC' row.")

er_auc <- pick_first_metric(er, "AUC")
qc_assert(!is.null(er_auc), "Exposure-response results missing metric == 'AUC' row.")

# ---- Population summary ----
dose_groups_text <- NARRATIVE_SLOTS$dose_groups_text

n_by_grp <- pkp |>
  dplyr::count(dosegrp) |>
  dplyr::arrange(match(as.character(dosegrp), c("Placebo", "Low", "Medium", "High")))

auc_means <- pkp |>
  dplyr::group_by(dosegrp) |>
  dplyr::summarize(auc_mean = mean(auc, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(match(as.character(dosegrp), c("Placebo", "Low", "Medium", "High")))

# ---- Human-readable lines ----
doseprop_line <- paste0(
  "Dose proportionality (AUC): β=",
  fmt_num(as.numeric(dp_auc$slope_beta), 3),
  " (95% CI [", fmt_num(as.numeric(dp_auc$ci_low), 3), ", ", fmt_num(as.numeric(dp_auc$ci_high), 3), "])",
  "; p=", fmt_p(as.numeric(dp_auc$p_value)), "."
)

exresp_line <- paste0(
  "Exposure–response (AUC): OR=",
  fmt_ci(as.numeric(er_auc$odds_ratio), as.numeric(er_auc$or_ci_low), as.numeric(er_auc$or_ci_high)),
  "; p=", fmt_p(as.numeric(er_auc$p_value)), "."
)

# Safety lines
# sf expects ae_any_pct and dlt_pct
sf$ae_any_pct <- as.numeric(sf$ae_any_pct)
sf$dlt_pct <- as.numeric(sf$dlt_pct)

ae_line <- paste0(
  "AE rates by dose group: ",
  paste0(sf$dosegrp, "=", sprintf("%.1f%%", sf$ae_any_pct), collapse = ", "),
  "."
)

dlt_line <- paste0(
  "DLT rates by dose group: ",
  paste0(sf$dosegrp, "=", sprintf("%.1f%%", sf$dlt_pct), collapse = ", "),
  "."
)

# ---- Traceability (no re-run) ----
trace <- trace_lines(c(
  "PK params (RDS)"                = pk_params_rds,
  "Dose proportionality (CSV)"     = dp_csv,
  "Exposure-response (CSV)"        = er_csv,
  "Safety summary (CSV)"           = safety_csv
))

# ---- Build narrative (Markdown) ----
has_conclusion <- !is.null(NARRATIVE_SLOTS$conclusion_sentence) &&
  length(NARRATIVE_SLOTS$conclusion_sentence) > 0

conclusion_text <- if (has_conclusion) {
  NARRATIVE_SLOTS$conclusion_sentence
} else {
  "These outputs provide an end-to-end demonstration of early-phase PK analysis and decision-support summaries using reproducible code and frozen artifacts."
}

lines <- c(
  paste0("# ", project_name, " Results Narrative"),
  "",
  paste0("Generated: ", stamp_time(run_time)),
  "",
  "## Executive Summary",
  paste0("- Dose proportionality (primary exposure AUC): ", doseprop_line),
  paste0("- Exposure–response (primary exposure AUC): ", exresp_line),
  "- Safety was summarized descriptively by dose group (AE and DLT).",
  "",
  "## Study Overview",
  "This early-phase dose-finding analysis evaluates pharmacokinetic exposure across dose levels and examines dose proportionality, exposure–response patterns, and safety by dose group.",
  "",
  "## Dose Groups",
  dose_groups_text,
  "",
  "## Analysis Set and Data Overview",
  paste0(
    "Subjects by dose group: ",
    paste0(as.character(n_by_grp$dosegrp), "=", n_by_grp$n, collapse = ", "),
    "."
  ),
  "",
  "## PK Exposure (AUC)",
  paste0(
    "Mean AUC by dose group: ",
    paste0(as.character(auc_means$dosegrp), "=", fmt_num(auc_means$auc_mean, 1), collapse = ", "),
    "."
  ),
  "",
  "## Primary Results",
  doseprop_line,
  "",
  "## Secondary Results",
  exresp_line,
  "",
  "## Safety",
  ae_line,
  dlt_line,
  "",
  "## Conclusion",
  conclusion_text,
  "",
  "## Appendix: Output Traceability (No Re-run)",
  "This narrative was created by reading the following frozen outputs (not by refitting models):",
  trace,
  "",
  "QC artifacts referenced:",
  paste0("- `", qc02, "`"),
  paste0("- `", qc04, "`"),
  paste0("- `", qc05, "`"),
  paste0("- `", qc06, "` (if present)")
)

out_md  <- file.path(PATHS$results, "RESULTS_NARRATIVE.md")
out_txt <- file.path(PATHS$results, "RESULTS_NARRATIVE.txt")
writeLines(lines, out_md,  useBytes = TRUE)
writeLines(lines, out_txt, useBytes = TRUE)

# ---- QC output ----
qc_path <- file.path(PATHS$results, "QC_08_results_narrative.txt")
qc_write(c(
  "QC: 08_results_narrative.R (Narrative layer)",
  paste0("Run at: ", stamp_time(run_time)),
  paste0("Project: ", project_name),
  paste0("Saved: ", out_md),
  paste0("Saved: ", out_txt)
), qc_path)

message("✅ Narrative written: ", out_md)
message("✅ QC written: ", qc_path)

# Also drop narrative into the reference packet (if it exists)
ref_misc <- file.path(PATHS$results, "packet_reference_output", "misc")
if (dir.exists(ref_misc)) {
  file.copy(out_md,  file.path(ref_misc, basename(out_md)),  overwrite = TRUE)
  file.copy(out_txt, file.path(ref_misc, basename(out_txt)), overwrite = TRUE)
}
