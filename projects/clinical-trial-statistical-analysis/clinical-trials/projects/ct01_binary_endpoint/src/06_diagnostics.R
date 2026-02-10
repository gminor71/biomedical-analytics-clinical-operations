# src/06_diagnostics.R
# Purpose: Diagnostics for CT01 primary logistic regression
# Outputs:
# - VIF table (csv)
# - Influence summary (csv)
# - Diagnostic figures (png)
# - QC report (txt)



# ---- Inputs ----
model_path <- file.path(PATHS$results, "primary_model.rds")
qc_assert(file.exists(model_path),
          "Missing primary model. Run 04_primary_analysis.R first.")

m <- readRDS(model_path)
qc_assert(inherits(m, "glm"), "QC: primary_model.rds is not a glm object.")
qc_assert(isTRUE(m$converged), "QC: primary glm did not converge.")

# We'll also rebuild the model dataset exactly (complete-case) to ensure consistency
data_path <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
qc_assert(file.exists(data_path),
          "Missing analysis-ready dataset. Run 02_data_cleaning.R first.")
df <- readRDS(data_path)

required_cols <- c("usubjid", "trt", "age", "sex", "baseline_severity", "event_bin")
qc_dataset_cleaning(df, required_cols = required_cols, trt_col = "trt", endpoint_col = "event_bin")

vars_model <- c("usubjid", "event_bin", "trt", "age", "sex", "baseline_severity")
df_model <- df |>
  dplyr::select(dplyr::all_of(vars_model)) |>
  tidyr::drop_na()

qc_assert(nrow(df_model) > 0, "QC: No complete-case rows available for diagnostics.")

# Confirm model formula matches expectation
coef_names <- names(coef(m))
qc_assert("trtActive" %in% coef_names)


# ---- Core diagnostic quantities ----
fitted_p <- fitted(m)                      # predicted probability
res_dev  <- residuals(m, type = "deviance")
res_pear <- residuals(m, type = "pearson")

# Hat values (leverage) and Cook's distance
lev <- hatvalues(m)
cooks <- cooks.distance(m)

# NEW: ensure row alignment
n <- nrow(df_model)
qc_assert(nrow(df_model) == length(fitted_p),
          "QC: row mismatch between df_model and model outputs.")

# Standardized residuals (approx)
res_dev_std <- res_dev / sqrt(1 - lev)

# Influence (dfbetas)
dfb <- dfbetas(m)
dfb_trt <- if ("trtActive" %in% colnames(dfb)) dfb[, "trtActive"] else rep(NA_real_, nrow(dfb))

# ---- VIF (multicollinearity) ----
# car::vif works for glm. For factors, it may return GVIF; we’ll convert to a comparable scalar.
vif_raw <- car::vif(m)

vif_tbl <- NULL
if (is.matrix(vif_raw)) {
  # GVIF format: GVIF, Df, GVIF^(1/(2*Df))
  vif_tbl <- data.frame(
    term = rownames(vif_raw),
    GVIF = vif_raw[, 1],
    Df   = vif_raw[, 2],
    GVIF_adj = vif_raw[, 3],
    row.names = NULL
  )
} else {
  vif_tbl <- data.frame(
    term = names(vif_raw),
    VIF = as.numeric(vif_raw),
    row.names = NULL
  )
}

vif_csv <- file.path(PATHS$tables, "diagnostics_vif.csv")
readr::write_csv(vif_tbl, vif_csv)

# ---- Influence summary table ----
# Rules of thumb (not hard rules):
# - High leverage: > 2p/n
# - Cook's D: > 4/n (flag)
p <- length(coef(m))
n <- nrow(df_model)
lev_thr <- 2 * p / n
cook_thr <- 4 / n

infl <- df_model |>
  dplyr::transmute(
    usubjid,
    fitted_p = fitted_p,
    res_dev = res_dev,
    res_pear = res_pear,
    leverage = lev,
    cooks_d = cooks,
    dfbeta_trtActive = dfb_trt,
    flag_high_leverage = leverage > lev_thr,
    flag_high_cooks = cooks_d > cook_thr
  ) |>
  dplyr::arrange(dplyr::desc(cooks_d))

infl_csv <- file.path(PATHS$results, "diagnostics_influence_summary.csv")
readr::write_csv(infl, infl_csv)

top10 <- infl |> dplyr::slice_head(n = 10)

# ---- Figures ----
# Build ggplot objects, then save via shared utils_visuals.R::save_png()

fig1 <- file.path(PATHS$figures, "diag_residuals_vs_fitted.png")
p1 <- ggplot2::ggplot(
  data = data.frame(fitted_p = fitted_p, res_dev = res_dev),
  ggplot2::aes(x = fitted_p, y = res_dev)
) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_hline(yintercept = 0, linetype = 2, color = COLORS$ref_line) +
  ggplot2::labs(
    title = "Deviance Residuals vs Fitted Probability",
    x = "Fitted probability",
    y = "Deviance residual"
  ) +
  THEME_CLINICAL()
save_png(fig1, p1)

fig2 <- file.path(PATHS$figures, "diag_std_residuals_qq.png")
x <- sort(res_dev_std)
qq <- stats::qnorm(ppoints(length(x)))
p2 <- ggplot2::ggplot(
  data = data.frame(theoretical = qq, observed = x),
  ggplot2::aes(x = theoretical, y = observed)
) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, color = COLORS$ref_line) +
  ggplot2::labs(
    title = "Q-Q Plot (Standardized Deviance Residuals)",
    x = "Theoretical quantiles",
    y = "Observed quantiles"
  ) +
  THEME_CLINICAL()
save_png(fig2, p2)

fig3 <- file.path(PATHS$figures, "diag_cooks_distance.png")
p3 <- ggplot2::ggplot(
  data = data.frame(i = seq_along(cooks), cooks_d = cooks),
  ggplot2::aes(x = i, y = cooks_d)
) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::geom_hline(yintercept = cook_thr, linetype = 2, color = COLORS$ref_line) +
  ggplot2::labs(
    title = "Cook's Distance (Influence)",
    subtitle = paste0("Dashed line = 4/n (", signif(cook_thr, 3), ")"),
    x = "Observation index (complete-case set)",
    y = "Cook's D"
  ) +
  THEME_CLINICAL()
save_png(fig3, p3)

fig4 <- file.path(PATHS$figures, "diag_leverage.png")
p4 <- ggplot2::ggplot(
  data = data.frame(i = seq_along(lev), leverage = lev),
  ggplot2::aes(x = i, y = leverage)
) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::geom_hline(yintercept = lev_thr, linetype = 2, color = COLORS$ref_line) +
  ggplot2::labs(
    title = "Leverage (Hat Values)",
    subtitle = paste0("Dashed line = 2p/n (", signif(lev_thr, 3), ")"),
    x = "Observation index (complete-case set)",
    y = "Leverage"
  ) +
  THEME_CLINICAL()
save_png(fig4, p4)


# ---- QC report ----
qc_path <- file.path(PATHS$results, "QC_06_diagnostics.txt")

vif_flag_line <- ""
if ("VIF" %in% names(vif_tbl)) {
  max_vif <- max(vif_tbl$VIF, na.rm = TRUE)
  vif_flag_line <- paste0("Max VIF: ", round(max_vif, 3), " (rule-of-thumb concern often > 5–10)")
} else if ("GVIF_adj" %in% names(vif_tbl)) {
  max_gvif <- max(vif_tbl$GVIF_adj, na.rm = TRUE)
  vif_flag_line <- paste0("Max GVIF^(1/(2*Df)): ", round(max_gvif, 3), " (rule-of-thumb concern often > 5–10)")
}

n_high_cook <- sum(infl$flag_high_cooks, na.rm = TRUE)
n_high_lev  <- sum(infl$flag_high_leverage, na.rm = TRUE)

qc_write(c(
  "QC: 06_diagnostics.R (Primary model diagnostics)",
  paste0("Run at: ", Sys.time()),
  "",
  "Primary model: event_bin ~ trt + age + sex + baseline_severity (binomial/logit)",
  paste0("Complete-case N used here: ", n),
  paste0("Parameters p: ", p),
  "",
  "Influence thresholds (rules of thumb):",
  paste0("  Cook's D > 4/n = ", signif(cook_thr, 4)),
  paste0("  Leverage > 2p/n = ", signif(lev_thr, 4)),
  "",
  paste0("Flag counts: high Cook's D = ", n_high_cook, " ; high leverage = ", n_high_lev),
  "",
  "Top 10 by Cook's D:",
  qc_fmt_table(top10),
  "",
  "Collinearity (VIF/GVIF):",
  vif_flag_line,
  "",
  paste0("Saved VIF table: ", vif_csv),
  paste0("Saved influence table: ", infl_csv),
  "Saved figures:",
  paste0("  ", fig1),
  paste0("  ", fig2),
  paste0("  ", fig3),
  paste0("  ", fig4)
), qc_path)

message("✅ Saved VIF table: ", vif_csv)
message("✅ Saved influence summary: ", infl_csv)
message("✅ Saved diagnostic figures to figures/:")
message("   - ", basename(fig1))
message("   - ", basename(fig2))
message("   - ", basename(fig3))
message("   - ", basename(fig4))
message("✅ QC report written: ", qc_path)

