# src/06_diagnostics.R
# Purpose: Diagnostics for dose proportionality + exposure-response models + QC

source("src/00_setup.R")

diag_dir <- file.path(PATHS$results, "diagnostics")
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)

# Fail early if not writable
test_file <- file.path(diag_dir, ".__write_test__.txt")
ok <- tryCatch({ writeLines("test", test_file); TRUE }, error = function(e) FALSE)
if (!ok) stop("Diagnostics directory is not writable: ", diag_dir)
unlink(test_file)

# ---- Inputs ----
dp_models_path <- file.path(PATHS$results, "05_models_dose_proportionality.rds")
er_models_path <- file.path(PATHS$results, "05_models_exposure_response.rds")
pk_params_path <- file.path(PATHS$data_proc, "04_pk_params.rds")

stopifnot(file.exists(dp_models_path))
stopifnot(file.exists(er_models_path))
stopifnot(file.exists(pk_params_path))

dp_mods <- readRDS(dp_models_path)
er_mods <- readRDS(er_models_path)
pkp <- readRDS(pk_params_path)

fit_dp_auc  <- dp_mods$fit_dp_auc
fit_dp_cmax <- dp_mods$fit_dp_cmax

fit_er_auc  <- er_mods$fit_er_auc
fit_er_cmax <- er_mods$fit_er_cmax

safe_png <- function(path, width = 1200, height = 800, res = 150) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(filename = path, width = width, height = height, res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
}

# ---- Helper plots for lm ----
plot_lm_diagnostics <- function(fit, prefix) {
  # 1) Residuals vs Fitted
  safe_png(file.path(diag_dir, paste0(prefix, "_resid_fit.png")))
  plot(fitted(fit), resid(fit),
       xlab = "Fitted values", ylab = "Residuals",
       main = paste0(prefix, ": Residuals vs Fitted"))
  abline(h = 0, lty = 2)
  
  # 2) QQ plot
  safe_png(file.path(diag_dir, paste0(prefix, "_qq.png")))
  qqnorm(resid(fit), main = paste0(prefix, ": Normal Q-Q Plot"))
  qqline(resid(fit))
  
  # 3) Cook's distance
  cd <- cooks.distance(fit)
  safe_png(file.path(diag_dir, paste0(prefix, "_cook.png")))
  plot(cd, type = "h", ylab = "Cook's distance",
       main = paste0(prefix, ": Cook's Distance"))
  abline(h = 4/length(cd), lty = 2)
  
  cd
}

# ---- Helper diagnostics for glm (logistic) ----
plot_glm_diagnostics <- function(fit, data, exposure_var, prefix, n_bins = 10) {
  p_hat <- predict(fit, type = "response")
  
  df <- data.frame(
    p_hat = p_hat,
    y = data$resp_bin
  )
  df <- df[!is.na(df$y) & !is.na(df$p_hat), , drop = FALSE]
  
  # Binned calibration (observed vs predicted)
  df$bin <- NA
  
  qs <- quantile(df$p_hat, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
  qs <- unique(as.numeric(qs))
  
  if (length(qs) >= 3) {
    df$bin <- cut(df$p_hat, breaks = qs, include.lowest = TRUE)
  } else {
    # Fallback: equal-width bins if quantiles collapse
    rng <- range(df$p_hat, na.rm = TRUE)
    if (diff(rng) == 0) {
      df$bin <- factor("all")
    } else {
      df$bin <- cut(df$p_hat, breaks = seq(rng[1], rng[2], length.out = min(n_bins, 5) + 1),
                    include.lowest = TRUE)
    }
  }
  
  
  calib <- df |>
    dplyr::group_by(bin) |>
    dplyr::summarize(
      pred = mean(p_hat),
      obs  = mean(y),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  p_cal <- ggplot2::ggplot(calib, ggplot2::aes(x = pred, y = obs)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::labs(
      title = paste0(prefix, ": Calibration (binned)"),
      x = "Mean predicted probability",
      y = "Observed response rate"
    ) +
    ggplot2::theme_bw()
  
  ggplot2::ggsave(file.path(diag_dir, paste0(prefix, "_cal.png")), p_cal,
                  width = 7.5, height = 5.0, dpi = 180)
  
  # Influence: Cook's distance
  cd <- cooks.distance(fit)
  safe_png(file.path(diag_dir, paste0(prefix, "_cook.png")), width = 1200, height = 800, res = 150)
  plot(cd, type = "h", ylab = "Cook's distance",
       main = paste0(prefix, ": Cook's Distance"))
  abline(h = 4/length(cd), lty = 2)
  
  # Pred prob vs exposure
  if (exposure_var %in% names(data)) {
    df2 <- data.frame(exposure = data[[exposure_var]], p_hat = p_hat)
    df2 <- df2[!is.na(df2$exposure) & !is.na(df2$p_hat), , drop = FALSE]
    
    p_sc <- ggplot2::ggplot(df2, ggplot2::aes(x = exposure, y = p_hat)) +
      ggplot2::geom_point(alpha = 0.25) +
      ggplot2::geom_smooth(se = FALSE) +
      ggplot2::labs(
        title = paste0(prefix, ": Predicted probability vs ", exposure_var),
        x = exposure_var, y = "Predicted probability"
      ) +
      ggplot2::theme_bw()
    
    ggplot2::ggsave(file.path(diag_dir, paste0(prefix, "_predprob.png")), p_sc,
                    width = 7.5, height = 5.0, dpi = 180)
  }
  
  cd
}

# =========================
# Dose proportionality diagnostics (LM)
# =========================
cd_auc  <- plot_lm_diagnostics(fit_dp_auc,  "DoseProp_AUC")
cd_cmax <- plot_lm_diagnostics(fit_dp_cmax, "DoseProp_Cmax")

# =========================
# Exposure-response diagnostics (GLM)
# =========================
cd_er_auc  <- plot_glm_diagnostics(fit_er_auc,  pkp, exposure_var = "auc",  prefix = "ExposureResponse_AUC")
cd_er_cmax <- plot_glm_diagnostics(fit_er_cmax, pkp, exposure_var = "cmax", prefix = "ExposureResponse_Cmax")

# =========================
# Top influence tables
# =========================
top_k <- 10

# For LM models: we don't have usubjid in model frame (by design), so export observation index + values
dp_dat_auc  <- data.frame(dose = model.frame(fit_dp_auc)$`log(dose)`,  auc = model.frame(fit_dp_auc)$`log(auc)`)
dp_dat_cmax <- data.frame(dose = model.frame(fit_dp_cmax)$`log(dose)`, cmax = model.frame(fit_dp_cmax)$`log(cmax)`)

top_dp_auc <- tibble::tibble(
  obs = seq_along(cd_auc),
  cooks_distance = as.numeric(cd_auc)
) |>
  dplyr::mutate(log_dose = dp_dat_auc$dose, log_auc = dp_dat_auc$auc) |>
  dplyr::arrange(dplyr::desc(cooks_distance)) |>
  dplyr::slice_head(n = top_k)

top_dp_cmax <- tibble::tibble(
  obs = seq_along(cd_cmax),
  cooks_distance = as.numeric(cd_cmax)
) |>
  dplyr::mutate(log_dose = dp_dat_cmax$dose, log_cmax = dp_dat_cmax$cmax) |>
  dplyr::arrange(dplyr::desc(cooks_distance)) |>
  dplyr::slice_head(n = top_k)

top_er_auc <- tibble::tibble(
  usubjid = pkp$usubjid,
  cooks_distance = as.numeric(cd_er_auc)
) |>
  dplyr::arrange(dplyr::desc(cooks_distance)) |>
  dplyr::slice_head(n = top_k)

top_er_cmax <- tibble::tibble(
  usubjid = pkp$usubjid,
  cooks_distance = as.numeric(cd_er_cmax)
) |>
  dplyr::arrange(dplyr::desc(cooks_distance)) |>
  dplyr::slice_head(n = top_k)

readr::write_csv(top_dp_auc,  file.path(diag_dir, "TopInfluence_DoseProp_AUC.csv"))
readr::write_csv(top_dp_cmax, file.path(diag_dir, "TopInfluence_DoseProp_Cmax.csv"))
readr::write_csv(top_er_auc,  file.path(diag_dir, "TopInfluence_ExposureResponse_AUC.csv"))
readr::write_csv(top_er_cmax, file.path(diag_dir, "TopInfluence_ExposureResponse_Cmax.csv"))

# ---- Text note ----
note_path <- file.path(diag_dir, "06_diagnostics_summary.txt")
lines <- c(
  "CT04 Diagnostics Summary",
  "",
  "Files generated in results/diagnostics/:",
  "- Dose proportionality (AUC/Cmax): residual vs fitted, QQ plot, Cook's distance",
  "- Exposure-response (AUC/Cmax): binned calibration, Cook's distance, predicted probability vs exposure",
  "",
  "Top influential observations exported as CSV:",
  "- TopInfluence_DoseProp_AUC.csv",
  "- TopInfluence_DoseProp_Cmax.csv",
  "- TopInfluence_ExposureResponse_AUC.csv",
  "- TopInfluence_ExposureResponse_Cmax.csv",
  "",
  "Interpretation note:",
  "These diagnostics are intended to flag influential subjects and model-fit issues for follow-up.",
  "They do not automatically imply exclusion; sensitivity refits may be performed if warranted."
)
writeLines(lines, con = note_path, useBytes = TRUE)

message("✅ Diagnostics written to: ", diag_dir)

# ---- QC token ----
qc_path <- file.path(PATHS$results, "QC_06_diagnostics.txt")
writeLines(
  c(
    "QC_06_diagnostics: OK",
    paste0("Diagnostics dir exists: ", dir.exists(diag_dir)),
    paste0("Summary note exists: ", file.exists(note_path))
  ),
  qc_path
)
