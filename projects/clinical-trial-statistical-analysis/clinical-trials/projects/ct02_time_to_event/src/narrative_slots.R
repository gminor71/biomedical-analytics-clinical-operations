NARRATIVE_SLOTS <- list(
  study_id    = "CT02",
  study_title = "Time-to-Event Trial",
  
  endpoint_label = "time to event through Day 365",
  endpoint_var   = "time/status",
  
  estimand_primary =
    "hazard ratio (Active vs Control) from an adjusted Cox proportional hazards model",
  intercurrent_strategy =
    "treatment policy strategy; censoring handled via standard time-to-event methods",
  
  primary_model_label =
    "Cox proportional hazards model",
  
  covariates_text =
    "age, sex, and baseline severity",
  
  contrast_label = "Active vs Control",
  
  term_trt = "trtActive",
  
  supports_ph_diag  = TRUE,
  supports_emmeans  = FALSE
)
