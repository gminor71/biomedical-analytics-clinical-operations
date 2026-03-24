# ct01_binary_endpoint_trial_analysis/design/01_simulate_trial.R
# Purpose:
# Simulate a two-arm randomized Phase II clinical trial with a binary endpoint.
# Example endpoint: response / non-response.
#
# Outputs:
# - design/results/simulation_results.csv
# - design/results/simulation_summary.csv

rm(list = ls())

source("design/00_design_parameters.R")

set.seed(design_params$seed)

nsim <- design_params$nsim
alpha <- design_params$alpha
n_per_arm <- design_params$n_per_arm
p_control <- design_params$p_control
p_treatment <- design_params$p_treatment

# ---------------------------
# Helper functions
# ---------------------------
simulate_one_trial <- function(n_per_arm, p_control, p_treatment, alpha = 0.05) {
  y_control   <- rbinom(n_per_arm, size = 1, prob = p_control)
  y_treatment <- rbinom(n_per_arm, size = 1, prob = p_treatment)
  
  event_control   <- sum(y_control)
  event_treatment <- sum(y_treatment)
  
  prop_control   <- mean(y_control)
  prop_treatment <- mean(y_treatment)
  
  risk_difference <- prop_treatment - prop_control
  
  test <- prop.test(
    x = c(event_treatment, event_control),
    n = c(n_per_arm, n_per_arm),
    correct = FALSE
  )
  
  dat <- data.frame(
    y = c(y_control, y_treatment),
    arm = c(rep(0, n_per_arm), rep(1, n_per_arm))
  )
  
  fit <- glm(y ~ arm, family = binomial(), data = dat)
  coef_est <- coef(summary(fit))
  
  log_or <- unname(coef(fit)["arm"])
  se_log_or <- coef_est["arm", "Std. Error"]
  or_est <- exp(log_or)
  or_lcl <- exp(log_or - 1.96 * se_log_or)
  or_ucl <- exp(log_or + 1.96 * se_log_or)
  
  out <- data.frame(
    n_per_arm = n_per_arm,
    p_control_true = p_control,
    p_treatment_true = p_treatment,
    events_control = event_control,
    events_treatment = event_treatment,
    prop_control = prop_control,
    prop_treatment = prop_treatment,
    risk_difference = risk_difference,
    p_value = unname(test$p.value),
    rd_lcl = unname(test$conf.int[1]),
    rd_ucl = unname(test$conf.int[2]),
    odds_ratio = or_est,
    or_lcl = or_lcl,
    or_ucl = or_ucl,
    reject_h0 = as.integer(test$p.value < alpha)
  )
  
  return(out)
}

summarize_simulation <- function(sim_df, true_rd) {
  data.frame(
    nsim = nrow(sim_df),
    mean_prop_control = mean(sim_df$prop_control),
    mean_prop_treatment = mean(sim_df$prop_treatment),
    mean_risk_difference = mean(sim_df$risk_difference),
    true_risk_difference = true_rd,
    bias_risk_difference = mean(sim_df$risk_difference) - true_rd,
    empirical_power = mean(sim_df$reject_h0),
    mean_odds_ratio = mean(sim_df$odds_ratio),
    median_odds_ratio = median(sim_df$odds_ratio)
  )
}

# ---------------------------
# Run simulation
# ---------------------------
sim_list <- vector("list", nsim)

for (i in seq_len(nsim)) {
  sim_list[[i]] <- simulate_one_trial(
    n_per_arm = n_per_arm,
    p_control = p_control,
    p_treatment = p_treatment,
    alpha = alpha
  )
}

sim_results <- do.call(rbind, sim_list)

sim_summary <- summarize_simulation(
  sim_df = sim_results,
  true_rd = p_treatment - p_control
)

# ---------------------------
# Save outputs
# ---------------------------
out_dir <- file.path("design", "results")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(
  sim_results,
  file = file.path(out_dir, "simulation_results.csv"),
  row.names = FALSE
)

write.csv(
  sim_summary,
  file = file.path(out_dir, "simulation_summary.csv"),
  row.names = FALSE
)

cat("\nSimulation complete.\n")
print(sim_summary)