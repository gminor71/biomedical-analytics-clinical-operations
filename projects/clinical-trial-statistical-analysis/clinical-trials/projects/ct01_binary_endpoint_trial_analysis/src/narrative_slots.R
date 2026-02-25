NARRATIVE_SLOTS <- list(
  study_id    = "CT01",
  study_title = "Binary Endpoint Trial",
  
  endpoint_label = "binary event occurrence through Day 365",
  endpoint_var   = "event_bin",
  
  estimand_primary = "odds of event by Day 365 (Active vs Control)",
  intercurrent_strategy = "treatment policy strategy (ITT); subjects analyzed as randomized",
  
  primary_model_label = "logistic regression model with treatment as primary predictor",
  covariates_text = "age, sex, and baseline severity",
  
  contrast_label = "Active vs Control",
  
  term_trt = "trtActive",
  
  supports_ph_diag  = FALSE,
  supports_emmeans  = FALSE
)
