NARRATIVE_SLOTS <- list(
  study_id    = "CT03",
  study_title = "Longitudinal Biomarker Trial",
  
  endpoint_label = "continuous biomarker measured repeatedly through Day 365",
  endpoint_var   = "biomarker",
  
  # Estimand / strategy (SAP-aligned narrative text)
  estimand_primary =
    "difference in biomarker trajectory over time between Active vs Control (treatment-by-time interaction)",
  intercurrent_strategy =
    "treatment policy strategy; missing data handled under MAR via maximum likelihood",
  
  # Model labeling (narrative)
  primary_model_label =
    "linear mixed-effects model with a treatment-by-time interaction",
  
  # Covariates (used in sentence templates)
  covariates_text =
    "age, sex, baseline severity, and BMI",
  
  # Contrast labeling
  contrast_label = "Active vs Control",
  
  # ---- Operational slots (used by 08_results_narrative.R for extraction) ----
  # Fixed-effect term names expected in the fitted model object
  term_trt   = "trtActive",
  term_slope = "trtActive:time30",
  
  # Unit text for the slope contrast (shows up in effect_line + sentence)
  slope_unit = "per 30 days",
  
  # Optional model-specific diagnostics sentence override
  diag_sentence_lme =
    "Model convergence and fit diagnostics were reviewed; estimation used maximum likelihood under a MAR assumption.",
  
  # Feature flags for optional narrative add-ons
  supports_ph_diag  = FALSE,
  supports_emmeans  = TRUE
)
