# src/06_diagnostics.R
# Purpose: Diagnostics for CT03 primary mixed model
# Outputs:
# - results/diagnostics_residuals_summary.csv
# - figures/diag_resid_vs_fitted.png
# - figures/diag_resid_qq.png
# - figures/diag_ranef_intercepts.png
# - results/QC_06_diagnostics.txt

model_path <- file.path(PATHS$results, "primary_model.rds")
qc_assert(file.exists(model_path), "Missing primary model. Run 04_primary_analysis.R first.")
m <- readRDS(model_path)
qc_assert(inherits(m, "lmerMod"), "QC: primary_model.rds is not an lmerMod object.")

# Model frame used by lmer (aligns to residuals/fitted)
mf <- model.frame(m)
n <- nrow(mf)
fitted_y <- fitted(m)
resid_y <- resid(m)

qc_assert(length(fitted_y) == n, "QC: fitted length mismatch.")
qc_assert(length(resid_y) == n, "QC: residual length mismatch.")

diag_tbl <- data.frame(
  fitted = as.numeric(fitted_y),
  resid = as.numeric(resid_y)
)

diag_csv <- file.path(PATHS$results, "diagnostics_residuals_summary.csv")
readr::write_csv(diag_tbl, diag_csv)

# ---- Figures (project standard) ----
p1 <- ggplot2::ggplot(diag_tbl, ggplot2::aes(x = fitted, y = resid)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_hline(yintercept = 0, linetype = 2) +
  ggplot2::labs(
    title = "Residuals vs Fitted",
    x = "Fitted biomarker",
    y = "Residual"
  ) +
  THEME_CLINICAL()

fig1 <- file.path(PATHS$figures, "diag_resid_vs_fitted.png")
save_png(fig1, p1)

x <- sort(diag_tbl$resid)
qq <- stats::qnorm(ppoints(length(x)))
p2 <- ggplot2::ggplot(data.frame(theoretical = qq, observed = x),
                      ggplot2::aes(x = theoretical, y = observed)) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
  ggplot2::labs(
    title = "Q-Q Plot of Residuals",
    x = "Theoretical quantiles",
    y = "Observed residuals"
  ) +
  THEME_CLINICAL()

fig2 <- file.path(PATHS$figures, "diag_resid_qq.png")
save_png(fig2, p2)

# Random intercepts
re <- lme4::ranef(m)$usubjid
re$usubjid <- rownames(re)
colnames(re)[1] <- "ranef_intercept"

p3 <- ggplot2::ggplot(re, ggplot2::aes(x = ranef_intercept)) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::labs(
    title = "Distribution of Subject Random Intercepts",
    x = "Random intercept",
    y = "Count"
  ) +
  THEME_CLINICAL()

fig3 <- file.path(PATHS$figures, "diag_ranef_intercepts.png")
save_png(fig3, p3)

# ---- QC report ----
qc_path <- file.path(PATHS$results, "QC_06_diagnostics.txt")

qc_write(c(
  "QC: 06_diagnostics.R (CT03 mixed model diagnostics)",
  paste0("Run at: ", Sys.time()),
  "",
  paste0("Model rows (mf): ", n),
  "",
  paste0("Saved residual table: ", diag_csv),
  "Saved figures:",
  paste0("  ", fig1),
  paste0("  ", fig2),
  paste0("  ", fig3)
), qc_path)

message("✅ Saved diagnostics residual table: ", diag_csv)
message("✅ Saved diagnostic figures to figures/:")
message("   - ", basename(fig1))
message("   - ", basename(fig2))
message("   - ", basename(fig3))
message("✅ QC report written: ", qc_path)
