# src/06_diagnostics.R
# Purpose: Diagnostics for CT02 primary Cox proportional hazards model
# Outputs:
# - tables/diagnostics_ph_test.csv
# - results/diagnostics_influence_summary.csv
# - figures/km_by_trt.png
# - figures/ph_schoenfeld_trt.png (and optional global)
# - results/QC_06_diagnostics.txt

if (!exists("PATHS")) stop("PATHS not found. Run source('src/00_setup.R') first (or use 99_run_all.R).")
qc_assert(exists("qc_write"), "QC helpers not loaded. Run source('src/00_setup.R') first (or use 99_run_all.R).")

# ---- Inputs ----
model_path <- file.path(PATHS$results, "primary_model.rds")
data_path  <- file.path(PATHS$data_proc, "02_analysis_ready.rds")

qc_assert(file.exists(model_path), "Missing primary model. Run 04_primary_analysis.R first.")
qc_assert(file.exists(data_path),  "Missing analysis-ready dataset. Run 02_data_cleaning.R first.")

m  <- readRDS(model_path)
df <- readRDS(data_path)

qc_assert(inherits(m, "coxph"), "QC: primary_model.rds is not a coxph object.")
qc_assert(all(c("time","status","trt") %in% names(df)), "QC: analysis-ready data must contain time/status/trt.")
qc_assert(all(df$status %in% c(0L,1L), na.rm = TRUE), "QC: status must be 0/1.")
qc_assert(sum(df$status, na.rm = TRUE) > 0, "QC: No events in dataset.")

# Recreate the *exact* model frame used by coxph (after NA handling)
mf <- model.frame(m)
n_mf <- nrow(mf)
qc_assert(n_mf > 0, "QC: model.frame(m) has 0 rows.")

# ---- Kaplan–Meier plot (by treatment) ----
km_path <- file.path(PATHS$figures, "km_by_trt.png")
png(km_path, width = 900, height = 650, res = 120)
on.exit(dev.off(), add = TRUE)

km_fit <- survival::survfit(survival::Surv(time, status) ~ trt, data = df)

# KM fit
fit_km <- survival::survfit(survival::Surv(time, status) ~ trt, data = df_model)

# Plot
p_km <- survminer::ggsurvplot(
  fit_km,
  data = df_model,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = FALSE,
  palette = c(COLORS$control, COLORS$active),
  ggtheme = THEME_CLINICAL()
)

# Save to file (ggsurvplot returns a list; $plot is the ggplot)
km_path <- file.path(PATHS$figures, "km_by_trt.png")
ggplot2::ggsave(km_path, plot = p_km$plot, width = 7.5, height = 5.2, dpi = 150)

# Optional: save risk table too (nice for reports)
km_rt_path <- file.path(PATHS$figures, "km_by_trt_risktable.png")
ggplot2::ggsave(km_rt_path, plot = p_km$table, width = 7.5, height = 2.8, dpi = 150)

# ---- PH assumption check: Schoenfeld residuals ----
zph <- survival::cox.zph(m, transform = "km")
zph_tbl <- as.data.frame(zph$table) |> tibble::rownames_to_column("term")

ph_csv <- file.path(PATHS$tables, "diagnostics_ph_test.csv")
readr::write_csv(zph_tbl, ph_csv)

# IMPORTANT: the term name is often "trtActive" (not "trt")
term_trt <- if ("trtActive" %in% zph_tbl$term) "trtActive" else "trt"

ph_trt_path <- file.path(PATHS$figures, "ph_schoenfeld_trt.png")
save_png_base(ph_trt_path, {
  plot(zph, var = "trt", main = "Schoenfeld Residuals: Treatment")
  abline(h = 0, lty = 2, col = "grey50")
})



# ---- Influence diagnostics (dfbeta / residuals) ----
infl <- residuals(m, type = "dfbeta")   # matrix, one column per coefficient
infl_df <- as.data.frame(infl)
infl_df$usubjid <- df$usubjid[match(rownames(mf), df$usubjid)]  # best-effort link (may be NA if mismatch)

# If the above match is imperfect, fall back to rownames(mf) as ID
infl_df$row_id <- rownames(mf)

# Focus on treatment influence if present
has_trt <- "trtActive" %in% names(infl_df)
dfbeta_trt <- if (has_trt) infl_df$trtActive else rep(NA_real_, nrow(infl_df))

# Simple flag rule of thumb: |dfbeta| > 2/sqrt(n)
thr <- 2 / sqrt(n_mf)
flag_high <- abs(dfbeta_trt) > thr

infl_out <- data.frame(
  row_id = infl_df$row_id,
  usubjid = infl_df$usubjid,
  dfbeta_trtActive = dfbeta_trt,
  flag_high_dfbeta = flag_high
)

# Sort by absolute influence
infl_out <- infl_out[order(-abs(infl_out$dfbeta_trtActive)), ]

infl_csv <- file.path(PATHS$results, "diagnostics_influence_summary.csv")
readr::write_csv(infl_out, infl_csv)

n_flag <- sum(infl_out$flag_high_dfbeta, na.rm = TRUE)

# ---- QC report ----
qc_path <- file.path(PATHS$results, "QC_06_diagnostics.txt")

# Pull PH p-values for quick flagging
# zph_tbl has columns like: chisq, df, p (depends on survival version)
# We'll try to locate a p-value column robustly.
p_col <- intersect(names(zph_tbl), c("p", "pvalue", "p-value", "Pr(>|z|)"))
p_col <- if (length(p_col) > 0) p_col[1] else NA_character_

ph_flag_line <- "PH test: (see diagnostics_ph_test.csv)"
if (!is.na(p_col) && "term" %in% names(zph_tbl)) {
  # Global row usually named "GLOBAL"
  global_p <- zph_tbl[zph_tbl$term == "GLOBAL", p_col, drop = TRUE]
  if (length(global_p) == 1) {
    ph_flag_line <- paste0("PH test GLOBAL p-value: ", signif(global_p, 4),
                           " (rule-of-thumb concern if < 0.05)")
  }
}

qc_write(c(
  "QC: 06_diagnostics.R (CT02 Cox model diagnostics)",
  paste0("Run at: ", Sys.time()),
  "",
  paste0("Model class: ", paste(class(m), collapse = ", ")),
  paste0("Complete-case N used by model (model.frame): ", n_mf),
  "",
  "Artifacts saved:",
  paste0("  KM plot: ", km_path),
  paste0("  PH test table: ", ph_csv),
  paste0("  Schoenfeld plot (trt): ", ph_trt_path),
  paste0("  Influence summary: ", infl_csv),
  "",
  ph_flag_line,
  "",
  paste0("Influence rule-of-thumb: |dfbeta_trtActive| > 2/sqrt(n) = ", signif(thr, 4)),
  paste0("Flagged observations: ", n_flag),
  "",
  "Top 10 by |dfbeta_trtActive|:",
  qc_fmt_table(utils::head(infl_out, 10))
), qc_path)

message("✅ Saved KM plot: ", km_path)
message("✅ Saved PH test table: ", ph_csv)
message("✅ Saved Schoenfeld plot: ", ph_trt_path)
message("✅ Saved influence summary: ", infl_csv)
message("✅ QC report written: ", qc_path)
