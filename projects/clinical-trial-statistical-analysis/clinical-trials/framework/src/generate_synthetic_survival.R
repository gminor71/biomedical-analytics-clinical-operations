# Shared synthetic survival trial generator
# Produces subject-level time-to-event data

generate_synthetic_survival <- function(
    n = 300,
    censor_rate = 0.25,
    max_followup = 365,
    seed = 123
) {
  set.seed(seed)
  
  usubjid <- sprintf("SUBJ%04d", 1:n)
  trt <- sample(c("Control", "Active"), n, replace = TRUE)
  age <- round(rnorm(n, mean = 60, sd = 10))
  sex <- sample(c("Male", "Female"), n, replace = TRUE)
  baseline_severity <- round(rnorm(n, mean = 50, sd = 10), 1)
  
  # True treatment effect (HR ~ 0.65)
  beta_trt <- log(0.65)
  beta_sev <- log(1.03)   # worse severity = higher hazard
  beta_age <- log(1.01)
  
  linpred <- 
    beta_trt * (trt == "Active") +
    beta_sev * (baseline_severity - 50) +
    beta_age * (age - 60)
  
  # Baseline hazard
  base_rate <- 1 / 200
  
  true_time <- rexp(n, rate = base_rate * exp(linpred))
  censor_time <- runif(n, 0, max_followup)
  
  time <- pmin(true_time, censor_time)
  status <- as.integer(true_time <= censor_time)
  
  # Enforce approx censoring rate
  target_events <- round(n * (1 - censor_rate))
  if (sum(status) > target_events) {
    idx <- which(status == 1)
    flip <- sample(idx, sum(status) - target_events)
    status[flip] <- 0
  }
  
  data.frame(
    usubjid,
    trt,
    age,
    sex,
    baseline_severity,
    time = round(time, 1),
    status
  )
}
