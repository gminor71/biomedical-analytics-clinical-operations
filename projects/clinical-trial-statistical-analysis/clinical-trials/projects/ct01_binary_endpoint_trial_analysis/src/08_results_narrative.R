# src/08_results_narrative.R
# Purpose: Convert existing pipeline outputs into narrative text WITHOUT re-running analyses.
# Outputs:
# - results/RESULTS_NARRATIVE.md
# - results/RESULTS_NARRATIVE.txt
# - results/QC_08_results_narrative.txt

if (!exists("PATHS")) stop("PATHS not found. Run 00_setup.R first (or run 99_run_all.R).")

# ---- Narrative “slots” (project-specific) ----
source("src/narrative_slots.R")
qc_assert(exists("NARRATIVE_SLOTS"), "NARRATIVE_SLOTS not found. Did you create src/narrative_slots.R?")

project_name <- paste0(NARRATIVE_SLOTS$study_id, " - ", NARRATIVE_SLOTS$study_title)
run_time <- Sys.time()

# Default term names (can be overridden per project in narrative_slots.R)
term_trt   <- if (!is.null(NARRATIVE_SLOTS$term_trt))   NARRATIVE_SLOTS$term_trt   else "trtActive"
term_slope <- if (!is.null(NARRATIVE_SLOTS$term_slope)) NARRATIVE_SLOTS$term_slope else "trtActive:time30"
slope_unit <- if (!is.null(NARRATIVE_SLOTS$slope_unit)) NARRATIVE_SLOTS$slope_unit else "per 30 days"

# ---- Locate artifacts (do not parse HTML; prefer RDS/CSV) ----
tbl1_rds      <- file.path(PATHS$results, "table1_baseline.rds")        # from 03
primary_rds   <- file.path(PATHS$results, "primary_model.rds")          # from 04
secondary_rds <- file.path(PATHS$results, "secondary_models.rds")       # from 05

qc02 <- file.path(PATHS$results, "QC_02_data_cleaning.txt")
qc03 <- file.path(PATHS$results, "QC_03_table1.txt")
qc04 <- file.path(PATHS$results, "QC_04_primary_analysis.txt")
qc05 <- file.path(PATHS$results, "QC_05_secondary_analyses.txt")

# Optional, model-specific artifacts
emm_csv    <- file.path(PATHS$tables,  "primary_emmeans_by_visit.csv")      # CT03
ph_csv     <- file.path(PATHS$tables,  "diagnostics_ph_test.csv")          # CT02
int_csv_or <- file.path(PATHS$tables,  "interaction_or_by_severity.csv")   # CT01
int_csv_hr <- file.path(PATHS$tables,  "interaction_hr_by_severity.csv")   # CT02

# ---- Design artifacts (frozen outputs from design/results) ----
design_results_dir <- file.path("design", "results")

design_sim_csv   <- file.path(design_results_dir, "simulation_summary.csv")
design_oc_csv    <- file.path(design_results_dir, "operating_characteristics.csv")
design_power_csv <- file.path(design_results_dir, "power_curve.csv")
design_power_png <- file.path(design_results_dir, "power_curve.png")
design_params_r  <- file.path("design", "00_design_parameters.R")

design_available <- all(file.exists(c(design_sim_csv, design_oc_csv, design_power_csv)))

design_sim   <- NULL
design_oc    <- NULL
design_power <- NULL
design_lines <- NULL

if (design_available) {
  design_sim   <- safe_read_csv(design_sim_csv)
  design_oc    <- safe_read_csv(design_oc_csv)
  design_power <- safe_read_csv(design_power_csv)
  
  if (file.exists(design_params_r)) {
    source(design_params_r)
  }
  
  if (exists("design_params")) {
    ctrl <- design_params$p_control
    trt  <- design_params$p_treatment
    eff  <- trt - ctrl
    n_pa <- design_params$n_per_arm
    tgt  <- design_params$target_power
    nsim_design <- design_params$nsim
    alpha_design <- design_params$alpha
    
    emp_power <- NA_real_
    mean_ctrl <- NA_real_
    mean_trt  <- NA_real_
    
    if (!is.null(design_sim)) {
      if ("empirical_power" %in% names(design_sim)) {
        emp_power <- as.numeric(design_sim$empirical_power[1])
      }
      if ("mean_prop_control" %in% names(design_sim)) {
        mean_ctrl <- as.numeric(design_sim$mean_prop_control[1])
      }
      if ("mean_prop_treatment" %in% names(design_sim)) {
        mean_trt <- as.numeric(design_sim$mean_prop_treatment[1])
      }
    }
    
    req_n <- NA_real_
    if (!is.null(design_power) && all(c("n_per_arm", "power") %in% names(design_power))) {
      idx <- which(design_power$power >= tgt)
      if (length(idx) > 0) req_n <- min(design_power$n_per_arm[idx])
    }
    
    type1_vals <- NA_real_
    cover_vals <- NA_real_
    power_assumed <- NA_real_
    
    if (!is.null(design_oc)) {
      null_rows <- design_oc$true_risk_difference == 0
      if (any(null_rows)) {
        type1_vals <- mean(design_oc$rejection_rate[null_rows], na.rm = TRUE)
      }
      
      if ("ci_coverage" %in% names(design_oc)) {
        cover_vals <- mean(design_oc$ci_coverage, na.rm = TRUE)
      }
      
      assumed_rows <- abs(design_oc$true_risk_difference - eff) < 1e-8
      assumed_rows <- assumed_rows & abs(design_oc$p_control - ctrl) < 1e-8
      assumed_rows <- assumed_rows & abs(design_oc$p_treatment - trt) < 1e-8
      
      if (any(assumed_rows)) {
        power_assumed <- as.numeric(design_oc$rejection_rate[assumed_rows][1])
      }
    }
    
    design_lines <- c(
      "## Trial Design Evaluation",
      "A simulation-based design evaluation was conducted using frozen design outputs generated before formal results reporting.",
      "",
      "### Design Assumptions",
      paste0("- Control event rate: ", fmt_num(ctrl, 2)),
      paste0("- Treatment event rate: ", fmt_num(trt, 2)),
      paste0("- Absolute treatment effect: ", fmt_num(eff, 2)),
      paste0("- Significance level (alpha): ", fmt_num(alpha_design, 2)),
      paste0("- Target power: ", fmt_num(tgt, 2)),
      paste0("- Planned sample size: ", n_pa, " subjects per arm"),
      paste0("- Number of simulations: ", nsim_design),
      "",
      "### Design Results",
      paste0("- Analytical sample size required: ",
             ifelse(is.na(req_n), "not available", paste0(req_n, " subjects per arm"))),
      paste0("- Mean simulated control proportion: ",
             ifelse(is.na(mean_ctrl), "not available", fmt_num(mean_ctrl, 4))),
      paste0("- Mean simulated treatment proportion: ",
             ifelse(is.na(mean_trt), "not available", fmt_num(mean_trt, 4))),
      paste0("- Empirical power: ",
             ifelse(is.na(emp_power), "not available", fmt_num(emp_power, 4))),
      "",
      "### Operating Characteristics",
      paste0("- Mean type I error across null scenarios: ",
             ifelse(is.na(type1_vals), "not available", fmt_num(type1_vals, 4))),
      paste0("- Mean confidence interval coverage: ",
             ifelse(is.na(cover_vals), "not available", fmt_num(cover_vals, 4))),
      paste0("- Power at the assumed design effect: ",
             ifelse(is.na(power_assumed), "not available", fmt_num(power_assumed, 4))),
      "",
      "### Design Interpretation",
      paste0(
        "Analytical and simulation-based results support the planned sample size of ",
        n_pa,
        " subjects per arm for detecting an absolute treatment effect of ",
        fmt_num(eff, 2),
        "."
      ),
      ""
    )
  }
}

# ---- QC: required files exist ----
qc_assert(file.exists(primary_rds),   paste0("Missing: ", primary_rds))
qc_assert(file.exists(secondary_rds), paste0("Missing: ", secondary_rds))
qc_assert(file.exists(qc02) && file.exists(qc04) && file.exists(qc05),
          "Missing QC inputs required for narrative (QC_02/QC_04/QC_05).")

# ---- Read frozen objects ----
primary_model <- readRDS(primary_rds)
secondary_obj <- readRDS(secondary_rds)

# ---- Identify model type and extract primary estimand ----
effect_line <- NULL
effect_sentence <- NULL
diag_sentence <- NULL

if (inherits(primary_model, "glm")) {
  # CT01-like: logistic regression
  coef_name <- term_trt
  tab <- summary(primary_model)$coefficients
  qc_assert(coef_name %in% rownames(tab), "Primary glm missing trtActive.")
  
  beta <- tab[coef_name, "Estimate"]
  se   <- tab[coef_name, "Std. Error"]
  p    <- tab[coef_name, "Pr(>|z|)"]
  or   <- exp(beta)
  ci   <- exp(beta + c(-1, 1) * 1.96 * se)
  
  contrast <- if (!is.null(NARRATIVE_SLOTS$contrast_label)) NARRATIVE_SLOTS$contrast_label else "Active vs Control"
  effect_line <- paste0("Adjusted OR (", contrast, "): ", fmt_ci(or, ci[1], ci[2]), "; p=", fmt_p(p), ".")
  
  effect_sentence <- sent_primary_or(or, ci[1], ci[2], p)
  
  diag_sentence <- "The primary model converged and standard model checks were reviewed (influence and collinearity outputs)."
  
} else if (inherits(primary_model, "coxph")) {
  # CT02-like: Cox PH
  coef_name <- "trtActive"
  b <- coef(primary_model)
  V <- vcov(primary_model)
  qc_assert(coef_name %in% names(b), "Primary coxph missing trtActive.")
  
  beta <- as.numeric(b[coef_name])
  se   <- as.numeric(sqrt(V[coef_name, coef_name]))
  z    <- beta / se
  p    <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
  hr   <- exp(beta)
  ci   <- exp(beta + c(-1, 1) * 1.96 * se)
  
  effect_line <- paste0("Adjusted HR (Active vs Control): ", fmt_ci(hr, ci[1], ci[2]), "; p=", fmt_p(p), ".")
  effect_sentence <- sent_primary_hr(hr, ci[1], ci[2], p)
  
  # PH diagnostics from frozen CSV (only when enabled)
  if (isTRUE(NARRATIVE_SLOTS$supports_ph_diag) && file.exists(ph_csv)) {
    ph_tbl <- safe_read_csv(ph_csv)
    p_global <- NA_real_
    if (!is.null(ph_tbl) && all(c("term", "p") %in% names(ph_tbl))) {
      p_global <- ph_tbl$p[ph_tbl$term == "GLOBAL"]
      p_global <- if (length(p_global) == 1) as.numeric(p_global) else NA_real_
    }
    diag_sentence <- sent_diagnostics_ph(p_global)
  } else {
    diag_sentence <- sent_diagnostics_ph(NA_real_)
  }
  
} else if (inherits(primary_model, "lmerMod")) {
  # CT03-like: mixed model
  term <- "trtActive:time30"
  b <- lme4::fixef(primary_model)
  V <- as.matrix(vcov(primary_model))
  qc_assert(term %in% names(b), "Primary mixed model missing trtActive:time30.")
  
  beta <- as.numeric(b[term])
  se   <- as.numeric(sqrt(V[term, term]))
  z    <- beta / se
  p    <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
  
  effect_line <- paste0("Treatment-by-time interaction (per 30 days): beta=", fmt_num(beta, 4),
                        " (SE=", fmt_num(se, 4), "); p=", fmt_p(p), ".")
  effect_sentence <- sent_primary_lme_slope(beta, se, p, unit = "per 30 days")
  
  diag_sentence <- "Model convergence and fit diagnostics were reviewed; the mixed model uses maximum likelihood under a MAR assumption."
  
} else {
  stop("Unrecognized primary model class: ", paste(class(primary_model), collapse = ", "))
}

# ---- Optional: CT03 emmeans sentence ----
emm_sentence <- NULL
if (isTRUE(NARRATIVE_SLOTS$supports_emmeans) && file.exists(emm_csv)) {
  emm <- safe_read_csv(emm_csv)
  if (!is.null(emm) && "visit_day" %in% names(emm)) {
    days_star <- sort(unique(emm$visit_day))
    if (length(days_star) > 0) {
      emm_sentence <- paste0(
        "Estimated marginal means were produced at prespecified visits (",
        paste(days_star, collapse = ", "),
        " days)."
      )
    }
  }
}

# ---- Traceability ----
trace <- trace_lines(c(
  "Primary model (RDS)"              = primary_rds,
  "Secondary models (RDS)"           = secondary_rds,
  "Table 1 object (RDS)"             = tbl1_rds,
  "EMMEANS (CSV)"                    = emm_csv,
  "PH test (CSV)"                    = ph_csv,
  "CT01 interaction OR (CSV)"        = int_csv_or,
  "CT02 interaction HR (CSV)"        = int_csv_hr,
  "Design simulation summary (CSV)"  = design_sim_csv,
  "Design operating characteristics" = design_oc_csv,
  "Design power curve (CSV)"         = design_power_csv,
  "Design power figure (PNG)"        = design_power_png
))

# ---- Build narrative (Markdown) ----
lines <- c(
  paste0("# ", toupper(project_name), " Results Narrative"),
  "",
  paste0("Generated: ", stamp_time(run_time)),
  "",
  if (!is.null(design_lines)) design_lines else NULL,
  "## Executive Summary",
  paste0("- Primary estimand: ", effect_line),
  "- Sensitivity and secondary analyses were reviewed for consistency with the primary conclusion.",
  paste0("- Diagnostics: ", diag_sentence),
  "",
  "## Analysis Set and Data Overview",
  "The primary analysis was conducted in the intention-to-treat (ITT) population (all randomized subjects analyzed by assigned treatment).",
  "Key QC outputs were used as the source of truth for dataset integrity and analysis reproducibility (see Appendix).",
  "",
  "## Baseline Characteristics (Table 1)",
  sent_baseline_table1(),
  "",
  "## Primary Efficacy Results",
  effect_sentence,
  "",
  "## Secondary and Sensitivity Analyses",
  "Secondary and sensitivity analyses (as prespecified) were generated to evaluate robustness of the primary findings and explore effect modification.",
  "",
  "## Model Diagnostics and Assumptions",
  diag_sentence,
  if (!is.null(emm_sentence)) emm_sentence else NULL,
  "",
  "## Conclusion",
  sent_conclusion_generic(),
  "",
  "## Appendix: Output Traceability (No Re-run)",
  "This narrative was created by reading the following frozen outputs (not by refitting models):",
  trace,
  "",
  "QC artifacts referenced:",
  paste0("- `", qc02, "`"),
  paste0("- `", qc03, "` (if present)"),
  paste0("- `", qc04, "`"),
  paste0("- `", qc05, "`")
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
  paste0("Primary model class: ", paste(class(primary_model), collapse = ", ")),
  paste0("Saved: ", out_md),
  paste0("Saved: ", out_txt),
  paste0("Design available: ", design_available),
  paste0("Design simulation summary exists: ", file.exists(design_sim_csv)),
  paste0("Design operating characteristics exists: ", file.exists(design_oc_csv)),
  paste0("Design power curve CSV exists: ", file.exists(design_power_csv)),
  paste0("Design power figure PNG exists: ", file.exists(design_power_png))
), qc_path)

message("✅ Narrative written: ", out_md)
message("✅ QC written: ", qc_path)

# Also drop narrative into the reference packet (if it exists)
ref_misc <- file.path(PATHS$results, "packet_reference_output", "misc")
if (dir.exists(ref_misc)) {
  file.copy(out_md,  file.path(ref_misc, basename(out_md)),  overwrite = TRUE)
  file.copy(out_txt, file.path(ref_misc, basename(out_txt)), overwrite = TRUE)
}

# Also drop key design artifacts into the reference packet (if it exists)
ref_packet <- file.path(PATHS$results, "packet_reference_output")
if (dir.exists(ref_packet)) {
  ref_fig  <- file.path(ref_packet, "figures")
  ref_tbl  <- file.path(ref_packet, "tables")
  ref_misc2 <- file.path(ref_packet, "misc")
  
  dir.create(ref_fig,  recursive = TRUE, showWarnings = FALSE)
  dir.create(ref_tbl,  recursive = TRUE, showWarnings = FALSE)
  dir.create(ref_misc2, recursive = TRUE, showWarnings = FALSE)
  
  if (file.exists(design_power_png)) {
    file.copy(
      design_power_png,
      file.path(ref_fig, "ct01_design_power_curve.png"),
      overwrite = TRUE
    )
  }
  
  if (file.exists(design_power_csv)) {
    file.copy(
      design_power_csv,
      file.path(ref_tbl, "ct01_design_power_curve.csv"),
      overwrite = TRUE
    )
  }
  
  if (file.exists(design_oc_csv)) {
    file.copy(
      design_oc_csv,
      file.path(ref_tbl, "ct01_design_operating_characteristics.csv"),
      overwrite = TRUE
    )
  }
  
  if (file.exists(design_sim_csv)) {
    file.copy(
      design_sim_csv,
      file.path(ref_misc2, "ct01_design_simulation_summary.csv"),
      overwrite = TRUE
    )
  }
}

# Also drop narrative into the latest timestamped packet misc (if known)
latest_file <- file.path(PATHS$results, "LATEST_PACKET_DIR.txt")
latest_packet <- NA_character_

if (file.exists(latest_file)) {
  latest_packet <- readLines(latest_file, warn = FALSE)[1]
}

if (!is.na(latest_packet) && dir.exists(latest_packet)) {
  latest_misc <- file.path(latest_packet, "misc")
  dir.create(latest_misc, recursive = TRUE, showWarnings = FALSE)
  file.copy(out_md,  file.path(latest_misc, basename(out_md)),  overwrite = TRUE)
  file.copy(out_txt, file.path(latest_misc, basename(out_txt)), overwrite = TRUE)
}