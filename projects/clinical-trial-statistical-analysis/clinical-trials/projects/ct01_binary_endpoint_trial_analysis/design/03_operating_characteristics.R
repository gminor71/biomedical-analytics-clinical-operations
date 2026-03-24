# ct01_binary_endpoint_trial_analysis/design/03_operating_characteristics.R
# Purpose:
# Evaluate operating characteristics across multiple ct01 design scenarios:
# - type I error
# - power
# - bias
# - CI coverage
#
# Outputs:
# - design/results/operating_characteristics.csv

rm(list = ls())

source("design/00_design_parameters.R")

set.seed(design_params$seed)

nsim <- design_params$nsim
alpha <- design_params$alpha
n_per_arm <- design_params$n_per_arm

scenario_grid <- expand.grid(
  p_control = c(0.30, 0.35, 0.40),
  treatment_effect = c(0.00, 0.10, 0.15, 0.20)
)

scenario_grid$p_treatment <- scenario_grid$p_control + scenario_grid$treatment_effect

# Keep probabilities valid
scenario_grid <- subset(scenario_grid, p_treatment <= 0.95)

# ---------------------------
# Helper function
# ---------------------------
simulate_oc_one <- function(n_per_arm, p_control, p_treatment, alpha = 0.05) {
  y_control   <- rbinom(n_per_arm, size = 1, prob = p_control)
  y_treatment <- rbinom(n_per_arm, size = 1, prob = p_treatment)
  
  x_control   <- sum(y_control)
  x_treatment <- sum(y_treatment)
  
  prop_control   <- mean(y_control)
  prop_treatment <- mean(y_treatment)
  rd_est <- prop_treatment - prop_control
  true_rd <- p_treatment - p_control
  
  test <- prop.test(
    x = c(x_treatment, x_control),
    n = c(n_per_arm, n_per_arm),
    correct = FALSE
  )
  
  data.frame(
    rd_est = rd_est,
    true_rd = true_rd,
    p_value = unname(test$p.value),
    rd_lcl = unname(test$conf.int[1]),
    rd_ucl = unname(test$conf.int[2]),
    reject_h0 = as.integer(test$p.value < alpha),
    covered = as.integer(test$conf.int[1] <= true_rd & test$conf.int[2] >= true_rd)
  )
}

evaluate_scenario <- function(nsim, n_per_arm, p_control, p_treatment, alpha = 0.05) {
  sims <- vector("list", nsim)
  
  for (i in seq_len(nsim)) {
    sims[[i]] <- simulate_oc_one(
      n_per_arm = n_per_arm,
      p_control = p_control,
      p_treatment = p_treatment,
      alpha = alpha
    )
  }
  
  sim_df <- do.call(rbind, sims)
  true_rd <- p_treatment - p_control
  
  data.frame(
    n_per_arm = n_per_arm,
    nsim = nsim,
    p_control = p_control,
    p_treatment = p_treatment,
    true_risk_difference = true_rd,
    mean_estimated_rd = mean(sim_df$rd_est),
    bias_rd = mean(sim_df$rd_est) - true_rd,
    rejection_rate = mean(sim_df$reject_h0),
    ci_coverage = mean(sim_df$covered)
  )
}

# ---------------------------
# Run all scenarios
# ---------------------------
oc_results <- vector("list", nrow(scenario_grid))

for (i in seq_len(nrow(scenario_grid))) {
  oc_results[[i]] <- evaluate_scenario(
    nsim = nsim,
    n_per_arm = n_per_arm,
    p_control = scenario_grid$p_control[i],
    p_treatment = scenario_grid$p_treatment[i],
    alpha = alpha
  )
}

oc_df <- do.call(rbind, oc_results)

oc_df$metric_label <- ifelse(
  oc_df$true_risk_difference == 0,
  "Type I Error Scenario",
  "Power Scenario"
)

# ---------------------------
# Save outputs
# ---------------------------
out_dir <- file.path("design", "results")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(
  oc_df,
  file = file.path(out_dir, "operating_characteristics.csv"),
  row.names = FALSE
)

cat("\nOperating characteristics complete.\n")
print(oc_df)