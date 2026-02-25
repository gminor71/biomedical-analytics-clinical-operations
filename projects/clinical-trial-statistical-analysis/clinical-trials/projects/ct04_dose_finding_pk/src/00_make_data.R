# src/00_make_data.R
# Generate CT04 synthetic datasets that support:
# - PK parameter derivation (AUC/Cmax)
# - Dose proportionality assessment
# - Exposure-response (binary responder)
# - Safety by dose (AE/DLT)
#
# Uses trial_subject_level.csv as a shared "population spine" and adds CT04-specific fields.

source("src/00_setup.R")

set.seed(20260212)

# ---- Inputs ----
spine_path <- file.path(PATHS$data_raw, "trial_subject_level.csv")
if (!file.exists(spine_path)) {
  stop(
    "Missing shared dataset: ", spine_path, "\n",
    "Run framework synthetic generator to create it (framework/src/00_make_synthetic_data.R)."
  )
}

df_spine <- readr::read_csv(spine_path, show_col_types = FALSE) |>
  dplyr::mutate(
    usubjid = as.character(usubjid),
    site    = factor(site),
    trt     = factor(trt, levels = c("Control", "Active")),
    age     = as.numeric(age),
    sex     = factor(sex),
    bmi     = as.numeric(bmi),
    baseline_severity = as.numeric(baseline_severity)
  )

# ---- Dose group assignment (Placebo + Low/Medium/High) ----
active_levels <- c("Low", "Medium", "High")
dose_map <- c(Placebo = 0, Low = 5, Medium = 10, High = 20)

df_ct04 <- df_spine |>
  dplyr::mutate(
    dosegrp = dplyr::case_when(
      trt == "Control" ~ "Placebo",
      trt == "Active"  ~ NA_character_
    )
  )

idx_active <- which(df_ct04$trt == "Active")
if (length(idx_active) == 0) stop("No Active subjects in spine dataset; cannot assign dose groups.")

df_ct04$dosegrp[idx_active] <- sample(active_levels, size = length(idx_active), replace = TRUE)

df_ct04 <- df_ct04 |>
  dplyr::mutate(
    dosegrp = factor(dosegrp, levels = c("Placebo", "Low", "Medium", "High")),
    dose    = unname(dose_map[as.character(dosegrp)]),
    pk_pop  = 1L
  )

stopifnot("dose" %in% names(df_ct04))
stopifnot(!any(is.na(df_ct04$dose)))

message("DEBUG: df_ct04 has dose? ", "dose" %in% names(df_ct04))
message("DEBUG: dose summary: ", paste(utils::head(df_ct04$dose, 3), collapse = ", "))

# ---- PK long data (single dose; hours post-dose) ----
time_pts <- c(0, 0.5, 1, 2, 4, 6, 8, 12, 24)

n <- nrow(df_ct04)

# Subject variability: ka, ke, V (lognormal)
V_i  <- rlnorm(n, meanlog = log(30), sdlog = 0.15)
ke_i <- rlnorm(n, meanlog = log(0.20), sdlog = 0.12)
ka_i <- rlnorm(n, meanlog = log(1.20), sdlog = 0.10)

# One-compartment oral concentration model
pk_conc_oral_1c <- function(time, dose, ka, ke, V, F = 1) {
  # Vectorized 1-compartment oral PK concentration (supports vector inputs)
  denom <- ka - ke
  
  # Avoid divide-by-zero when ka ~ ke
  denom_safe <- ifelse(abs(denom) < 1e-8, ifelse(denom >= 0, 1e-8, -1e-8), denom)
  
  conc <- (F * dose * ka) / (V * denom_safe) * (exp(-ke * time) - exp(-ka * time))
  
  # Placebo / zero dose rows -> 0 concentration
  conc <- ifelse(dose <= 0, 0, conc)
  
  # Guard against tiny negatives from numeric error
  pmax(0, conc)
}

stopifnot("dose" %in% names(df_ct04))

# Start from the subject-level rows that already include dose
pk_base <- df_ct04 |>
  dplyr::select(usubjid, dosegrp, dose) |>
  dplyr::mutate(V = V_i, ke = ke_i, ka = ka_i)

# Cross-join with time points (base R: no NSE issues)
df_pk_long <- merge(
  pk_base,
  data.frame(time = time_pts),
  by = NULL
)

# Compute concentrations (use explicit column references)
df_pk_long$conc_true <- pk_conc_oral_1c(
  time = df_pk_long$time,
  dose = df_pk_long$dose,
  ka   = df_pk_long$ka,
  ke   = df_pk_long$ke,
  V    = df_pk_long$V
)

df_pk_long$conc <- pmax(
  0,
  df_pk_long$conc_true * exp(rnorm(nrow(df_pk_long), mean = 0, sd = 0.20))
)

df_pk_long <- df_pk_long |>
  dplyr::select(usubjid, dosegrp, dose, time, conc)


# ---- Derive exposure for synthetic endpoint generation (AUC/Cmax) ----
auc_trapz <- function(time, conc) {
  o <- order(time)
  t <- time[o]; c <- conc[o]
  sum(diff(t) * (head(c, -1) + tail(c, -1)) / 2, na.rm = TRUE)
}

df_pk_params_gen <- df_pk_long |>
  dplyr::group_by(usubjid) |>
  dplyr::summarize(
    auc  = auc_trapz(time, conc),
    cmax = max(conc, na.rm = TRUE),
    .groups = "drop"
  )

df_ct04 <- df_ct04 |>
  dplyr::left_join(df_pk_params_gen, by = "usubjid")

# ---- CT04-specific response & safety endpoints ----
# Responder: probability increases with AUC, decreases with baseline severity
linpred_resp <- with(df_ct04,
                     -1.2 +
                       0.004 * auc -
                       0.015 * (baseline_severity - 50) +
                       0.01  * (age - 58)
)
p_resp <- plogis(linpred_resp)
resp_bin <- rbinom(n, 1, p_resp)

# AE and DLT increase with dose/exposure (DLT rarer)
p_resp <- plogis(-1.6 + 0.0025 * df_ct04$auc)

p_ae <- plogis(
  -1.2 +
    0.04 * (df_ct04$dose/5) +
    0.002 * df_ct04$auc
)

p_dlt <- plogis(
  -5.0 +
    0.0035 * df_ct04$auc
)

ae_any <- rbinom(n, 1, p_ae)
dlt    <- rbinom(n, 1, p_dlt)

df_ct04 <- df_ct04 |>
  dplyr::mutate(
    resp_bin = resp_bin,
    ae_any   = ae_any,
    dlt      = dlt
  )

# ---- Write outputs to data/raw (CT04-specific files) ----
out_subj <- file.path(PATHS$data_raw, "ct04_subject_level.csv")
out_pk   <- file.path(PATHS$data_raw, "ct04_pk_long.csv")

readr::write_csv(df_ct04, out_subj)
readr::write_csv(df_pk_long, out_pk)

message("✅ Wrote CT04 subject-level dataset: ", out_subj)
message("✅ Wrote CT04 PK long dataset: ", out_pk)

# ---- QC file ----
qc_path <- file.path(PATHS$results, "QC_00_make_data.txt")
qc_lines <- c(
  "QC_00_make_data: OK",
  paste0("Subjects: ", nrow(df_ct04)),
  paste0("PK rows: ", nrow(df_pk_long)),
  paste0("Dose groups: ", paste(levels(df_ct04$dosegrp), collapse = ", ")),
  paste0("Counts by dosegrp: ",
         paste0(names(table(df_ct04$dosegrp)), "=", as.integer(table(df_ct04$dosegrp)), collapse = ", "))
)
writeLines(qc_lines, qc_path, useBytes = TRUE)
