# ct00_design/00_design_parameters.R
# Purpose:
# Centralized parameter definitions for trial design simulations.
# All scripts in ct00_design source this file to ensure consistency.

design_params <- list(
  seed = 2026,
  nsim = 5000,
  alpha = 0.05,
  target_power = 0.80,
  n_per_arm = 120,
  p_control = 0.30,
  p_treatment = 0.45
)