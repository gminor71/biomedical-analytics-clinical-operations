# src/00_make_synthetic_data.R
# Generate a shared synthetic RCT dataset that supports:
# - Binary endpoint analysis
# - Time-to-event analysis
# - Longitudinal repeated measures analysis

source("src/00_setup.R")

set.seed(20260109)

# ---- User-tunable parameters ----
n <- 500                 # number of subjects
n_sites <- 10            # number of trial sites
followup_days <- 365     # maximum follow-up window
visit_days <- c(0, 30, 90, 180, 365)  # longitudinal visit schedule

# Treatment effect magnitudes (adjust to make signal easier/harder)
log_odds_trt <- -0.50    # binary endpoint treatment effect (Active vs Control)
log_hr_trt   <- -0.35    # TTE treatment effect on hazard (Active vs Control)
delta_slope  <- -0.06    # longitudinal effect on slope (Active vs Control)

# ---- Subject-level baseline ----
df_subj <- tibble::tibble(
  usubjid = sprintf("SUBJ-%04d", 1:n),
  site    = factor(sample(sprintf("SITE%02d", 1:n_sites), n, replace = TRUE)),
  trt     = factor(sample(c("Control", "Active"), n, replace = TRUE), levels = c("Control", "Active")),
  age     = round(pmin(pmax(rnorm(n, mean = 58, sd = 12), 18), 90)),
  sex     = factor(sample(c("F", "M"), n, replace = TRUE)),
  bmi     = round(pmin(pmax(rnorm(n, mean = 29, sd = 6), 16), 55), 1),
  baseline_severity = round(pmin(pmax(rnorm(n, mean = 50, sd = 10), 10), 90), 1)
)

# Site-level random effect (mimics operational differences)
site_re <- rnorm(n_sites, mean = 0, sd = 0.25)
names(site_re) <- levels(df_subj$site)
df_subj <- df_subj |>
  dplyr::mutate(site_effect = site_re[as.character(site)])

# ---- Binary endpoint (e.g., event by Day 365) ----
# Logistic model for event probability
linpred_bin <- with(df_subj,
                    -2.1 +
                      0.030 * (baseline_severity - 50) +
                      0.015 * (age - 58) +
                      0.040 * (bmi - 29) +
                      ifelse(sex == "M", 0.10, 0) +
                      ifelse(trt == "Active", log_odds_trt, 0) +
                      site_effect
)

p_event <- plogis(linpred_bin)
event_bin <- rbinom(n, 1, p_event)

# ---- Time-to-event (relapse time) with censoring ----
# Exponential survival with subject-specific hazard
# Hazard increases with severity/age and decreases with treatment
base_haz <- 1 / 220  # baseline daily hazard (~median 152 days if alone; adjust as desired)

linpred_hr <- with(df_subj,
                   0.020 * (baseline_severity - 50) +
                     0.010 * (age - 58) +
                     ifelse(trt == "Active", log_hr_trt, 0) +
                     0.30 * site_effect
)

hazard <- base_haz * exp(linpred_hr)

# True event time
tte_true <- rexp(n, rate = hazard)   # in days (continuous)

# Administrative censoring at follow-up window
censor_time <- rep(followup_days, n)

# Additional random loss-to-follow-up censoring
# More LTFU in higher severity (slightly) to make it realistic
ltfu_rate <- plogis(-2.8 + 0.02 * (df_subj$baseline_severity - 50))  # probability of LTFU before end
ltfu_flag <- rbinom(n, 1, ltfu_rate)
ltfu_time <- ifelse(ltfu_flag == 1, runif(n, min = 30, max = followup_days), followup_days)

# Observed time and event indicator
tte_obs <- pmin(tte_true, censor_time, ltfu_time)
tte_event <- as.integer(tte_true <= pmin(censor_time, ltfu_time))

# ---- Longitudinal biomarker over visits ----
# Biomarker decreases over time; treatment modifies slope; subject random intercept & slope
subj_re_int <- rnorm(n, mean = 0, sd = 6)   # random intercept
subj_re_slope <- rnorm(n, mean = 0, sd = 0.03) # random slope per day

# Fixed effects
beta0 <- 100
beta_sev <- 0.55         # higher severity -> higher baseline biomarker
beta_time <- -0.08       # average decline per day
beta_trt_intercept <- -1.0  # treatment effect on baseline (small)
beta_trt_slope <- delta_slope # treatment effect on slope (per day)

# Build long format visits
df_long <- tidyr::expand_grid(
  usubjid = df_subj$usubjid,
  visit_day = visit_days
) |>
  dplyr::left_join(df_subj, by = "usubjid") |>
  dplyr::mutate(
    # subject-specific trajectory
    true_value =
      beta0 +
      beta_sev * (baseline_severity - 50) +
      beta_time * visit_day +
      ifelse(trt == "Active", beta_trt_intercept, 0) +
      ifelse(trt == "Active", beta_trt_slope * visit_day, 0) +
      subj_re_int[match(usubjid, df_subj$usubjid)] +
      subj_re_slope[match(usubjid, df_subj$usubjid)] * visit_day +
      site_effect * 2.0,
    biomarker = true_value + rnorm(dplyr::n(), mean = 0, sd = 5)  # measurement noise
  ) |>
  dplyr::select(usubjid, site, trt, age, sex, bmi, baseline_severity, visit_day, biomarker)

# Add some realistic missingness at later visits (more missing for higher severity)
df_long <- df_long |>
  dplyr::mutate(
    miss_prob = plogis(-3.0 + 0.006 * (visit_day) + 0.03 * (baseline_severity - 50)),
    is_missing = rbinom(dplyr::n(), 1, miss_prob),
    biomarker = ifelse(is_missing == 1 & visit_day != 0, NA, biomarker)
  ) |>
  dplyr::select(-miss_prob, -is_missing)

# ---- Combine subject-level dataset ----
df_trial <- df_subj |>
  dplyr::mutate(
    event_bin = event_bin,
    tte_days  = round(tte_obs, 1),
    tte_event = tte_event
  ) |>
  dplyr::select(usubjid, site, trt, age, sex, bmi, baseline_severity, event_bin, tte_days, tte_event)

# ---- Write outputs to data/raw ----
out_subj <- file.path(PATHS$data_raw, "trial_subject_level.csv")
out_long <- file.path(PATHS$data_raw, "trial_longitudinal.csv")

readr::write_csv(df_trial, out_subj)
readr::write_csv(df_long, out_long)

message("✅ Wrote subject-level dataset: ", out_subj)
message("✅ Wrote longitudinal dataset: ", out_long)
message("Tip: Use trial_subject_level.csv for binary & TTE projects; trial_longitudinal.csv for mixed models.")
