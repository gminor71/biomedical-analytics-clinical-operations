source("src/00_setup.R")

source(file.path("..", "..", "shared", "src", "00_make_synthetic_data.R"))
df <- make_ct01_synthetic(n = 300, seed = 123)

out <- file.path(PATHS$data_raw, "trial_subject_level.csv")
readr::write_csv(df, out)

message("✅ Wrote CT01 synthetic raw data: ", out)
