NARRATIVE_SLOTS <- list(
  study_id    = "CT04",
  study_title = "Dose Finding and Pharmacokinetics",
  
  endpoint_label = "single-dose PK exposure (AUC, Cmax) and binary responder/safety outcomes",
  endpoint_var   = "auc",
  
  # Estimand / strategy (SAP-aligned narrative text)
  estimand_primary =
    "dose proportionality of exposure (AUC) across active dose levels using a log–log model excluding placebo",
  intercurrent_strategy =
    "not applicable (single-dose PK); descriptive summaries used for safety endpoints",
  
  # Model labeling (narrative)
  primary_model_label =
    "dose proportionality model: log(AUC) ~ log(Dose) (active doses only)",
  
  # Covariates (used in sentence templates; keep short and honest for this project)
  covariates_text =
    "no covariates in the primary model; exposure–response modeled univariately (AUC or Cmax)",
  
  # Contrast labeling
  contrast_label = "per log-dose increase (active doses only)",
  
  # Dose groups (project-specific text used in narrative)
  dose_groups_text =
    "Dose groups: Placebo (0 mg), Low (5 mg), Medium (10 mg), High (20 mg).",
  
  # ---- Operational slots (optional, for future expansion) ----
  # If you later choose to extract directly from saved model objects:
  term_dose_log = "log(dose)",   # lm slope term
  term_auc      = "auc",         # glm exposure term
  term_cmax     = "cmax",
  
  # Primary exposure metric
  primary_exposure = "AUC",
  
  # Optional diagnostics sentence override
  diag_sentence_generic =
    "Model diagnostics were reviewed using residual plots, calibration summaries, and influence (Cook’s distance).",
  
  # Feature flags for optional narrative add-ons
  supports_ph_diag  = FALSE,
  supports_emmeans  = FALSE
)
