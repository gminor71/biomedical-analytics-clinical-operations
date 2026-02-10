source("src/00_setup.R")

# Use the shared generator (path from project root)
source(file.path("..", "..", "shared", "src", "00_make_synthetic_data.R"))

message("✅ CT02 raw data generated via shared 00_make_synthetic_data.R")
message("   - data/raw/trial_subject_level.csv (use for CT02)")
message("   - data/raw/trial_longitudinal.csv (for future longitudinal project)")
