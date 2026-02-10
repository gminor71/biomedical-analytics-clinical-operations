# Clinical Trials – Biostatistics Workspace


This project is organized by endpoint type to reflect common clinical
trial statistical analysis workflows:

- **ct00_design** — study design, analysis planning, and TFL structure
- **ct01_binary_endpoint** — binary outcome modeling and analysis
- **ct02_time_to_event** — survival and time-to-event analysis workflows
- **ct03_longitudinal** — longitudinal and repeated-measures analysis

## Structure

- `projects/`
  - Active and completed clinical trial analyses
  - Each project is self-contained and reproducible

- `framework/`
  - Shared R utilities used across projects
  - Includes standardized path helpers and reusable functions

## How to start a new project

1. Copy `biostatistics-project-template/` into `projects/`
2. Rename it (e.g., `ct02_time_to_event`)
3. Add raw data to `data/raw/`
4. Run scripts in `src/` from `00` → `07`
5. Render reports from `rmd/`

## Conventions

- Raw data is never modified
- All analysis uses `.rds` files from `data/processed/`
- Primary analyses are prespecified
- Exploratory analyses are clearly labeled
