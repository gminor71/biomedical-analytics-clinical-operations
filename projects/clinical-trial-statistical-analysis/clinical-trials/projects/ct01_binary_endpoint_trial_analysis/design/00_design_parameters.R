# ct01_binary_endpoint_trial_analysis/design/00_design_parameters.R
# Purpose:
# Centralized parameter definitions for ct01 project-specific design evaluation.

design_params <- list(
  seed = 2026,
  nsim = 5000,
  alpha = 0.05,
  target_power = 0.80,
  n_per_arm = 96,
  p_control = 0.35,
  p_treatment = 0.55
)