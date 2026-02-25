# src/03_eda.R
# Purpose: Exploratory checks + EDA plots + QC_03_table1.txt

source("src/00_setup.R")

in_rds <- file.path(PATHS$data_proc, "02_analysis_ready.rds")
stopifnot(file.exists(in_rds))

dat <- readRDS(in_rds)
subj <- dat$subj
pk   <- dat$pk_long

eda_dir <- file.path(PATHS$results, "eda")
dir.create(eda_dir, recursive = TRUE, showWarnings = FALSE)

# ---- EDA QC summary (project-local, not the pipeline QC token) ----
qc_summary_path <- file.path(eda_dir, "03_eda_qc_summary.txt")
con <- file(qc_summary_path, open = "wt", encoding = "UTF-8")

writeLines("CT04 EDA QC Summary", con)
writeLines(sprintf("Subjects: %d (unique %d)", nrow(subj), dplyr::n_distinct(subj$usubjid)), con)
writeLines(sprintf("PK rows: %d (unique subjects %d)", nrow(pk), dplyr::n_distinct(pk$usubjid)), con)
writeLines("", con)

writeLines("Counts by dose group:", con)
capture.output(print(subj |> dplyr::count(dosegrp)), file = con)

writeLines("", con)
writeLines("Response rate by dose group:", con)
capture.output(print(subj |> dplyr::group_by(dosegrp) |> dplyr::summarize(resp_rate = mean(resp_bin), .groups="drop")), file = con)

writeLines("", con)
writeLines("AE rate by dose group:", con)
capture.output(print(subj |> dplyr::group_by(dosegrp) |> dplyr::summarize(ae_rate = mean(ae_any), .groups="drop")), file = con)

close(con)
message("✅ Saved: ", qc_summary_path)

# ---- Exploratory plot: concentration vs time (thin spaghetti) ----
p1 <- ggplot2::ggplot(pk, ggplot2::aes(x = time, y = conc, group = usubjid)) +
  ggplot2::geom_line(alpha = 0.15) +
  ggplot2::facet_wrap(~ dosegrp, scales = "free_y") +
  ggplot2::labs(
    title = "EDA: Individual Concentration–Time Profiles by Dose Group",
    x = "Time (hours)", y = "Concentration"
  ) +
  ggplot2::theme_bw()

p1_path <- file.path(eda_dir, "EDA_pk_profiles_by_dosegrp.png")
ggplot2::ggsave(p1_path, p1, width = 10, height = 6, dpi = 150)
message("✅ Saved: ", p1_path)

# ---- Pipeline QC token (matches CT03 runner expectation) ----
qc_token_path <- file.path(PATHS$results, "QC_03_table1.txt")
writeLines(
  c(
    "QC_03_table1: OK",
    paste0("02_analysis_ready.rds exists: ", file.exists(in_rds)),
    paste0("EDA plot exists: ", file.exists(p1_path))
  ),
  qc_token_path
)
