# ct01_binary_endpoint_trial_analysis/design/02_power_analysis.R
# Purpose:
# Perform analytical power and sample size calculations
# for a two-arm Phase II trial with a binary endpoint.
#
# Outputs:
# - design/results/power_curve.csv
# - design/results/power_curve.png

rm(list = ls())

source("design/00_design_parameters.R")

alpha <- design_params$alpha
target_power <- design_params$target_power
p_control <- design_params$p_control
p_treatment <- design_params$p_treatment

n_grid <- seq(40, 180, by = 5)

# ---------------------------
# Required sample size
# ---------------------------
ss <- power.prop.test(
  p1 = p_control,
  p2 = p_treatment,
  sig.level = alpha,
  power = target_power,
  alternative = "two.sided"
)

required_n_per_arm <- ceiling(ss$n)

cat("\nEstimated required sample size per arm:\n")
cat(required_n_per_arm, "\n\n")

# ---------------------------
# Power curve across sample sizes
# ---------------------------
power_vals <- sapply(n_grid, function(n) {
  power.prop.test(
    n = n,
    p1 = p_control,
    p2 = p_treatment,
    sig.level = alpha,
    alternative = "two.sided"
  )$power
})

power_df <- data.frame(
  n_per_arm = n_grid,
  power = power_vals
)

# ---------------------------
# Save outputs
# ---------------------------
out_dir <- file.path("design", "results")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(
  power_df,
  file = file.path(out_dir, "power_curve.csv"),
  row.names = FALSE
)

png(filename = file.path(out_dir, "power_curve.png"), width = 900, height = 600)
plot(
  power_df$n_per_arm,
  power_df$power,
  type = "b",
  pch = 19,
  xlab = "Sample Size Per Arm",
  ylab = "Power",
  main = "ct01 Analytical Power Curve: Binary Endpoint Trial"
)
abline(h = target_power, lty = 2)
abline(v = required_n_per_arm, lty = 2)
text(
  x = required_n_per_arm,
  y = min(power_df$power) + 0.05,
  labels = paste("n =", required_n_per_arm),
  pos = 4
)
dev.off()

cat("Power analysis complete.\n")