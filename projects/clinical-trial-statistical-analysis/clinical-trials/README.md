# Clinical Trial Statistical Analysis – Biostatistics Workspace

This repository demonstrates a structured clinical trial statistical analysis
workflow using reproducible Tables, Figures, and Listings (TFL) development 
aligned with common biostatistics practices.

In addition to traditional statistical analysis workflows, this workspace i
ncludes **agent-based decision systems** that translate statistical guidance 
into structured, executable logic.

---

## Analysis Workflow

```mermaid
flowchart LR
    A[ct00_design<br/>Study Design and Analysis Planning]
        --> B[Endpoint-Specific Analyses<br/>Binary, TTE, Longitudinal, Dose Finding/PK]

    B --> C[Tables, Figures, Listings<br/>Reproducible Outputs]

    C --> D[ct05_tte_methods_agent<br/>Decision System and Method Logic]
```


This workflow represents a layered clinical trial analysis system. 
Statistical methods are prespecified during study design and implemented 
through endpoint-specific analyses, producing reproducible Tables, Figures, 
and Listings (TFLs). These outputs can be further leveraged by 
decision-support systems, including agent-based frameworks, 
to operationalize statistical reasoning into structured method selection, 
validation, and reporting workflows.



---

## Extended System Architecture

This workspace now includes an additional layer:

```text
Study Design → Statistical Analysis → TFL Outputs → Decision Systems (Agents)
```

### Agent Layer

* `ct05_tte_methods_agent`

  * Rule-based decision engine for time-to-event analysis
  * Evaluates:

    * endpoint characteristics
    * proportional hazards (PH) assumption status
    * sample size and event rate considerations
  * Produces:

    * method recommendations
    * alternative modeling strategies
    * structured reports and batch summaries

This represents a transition from:

* **analysis execution** → to → **analysis decision support**

---

## Study-Level Workflow

This workspace represents a single clinical trial analysis environment
organized by endpoint type. Analyses follow a common workflow:

1. Study design and analysis planning (`ct00_design`)
2. Endpoint-specific statistical analysis

   * Binary endpoints (`ct01_binary_endpoint`)
   * Time-to-event endpoints (`ct02_time_to_event`)
   * Longitudinal endpoints (`ct03_longitudinal`)
3. Generation of reproducible Tables, Figures, and Listings (TFLs)
4. Decision support and method selection (`ct05_tte_methods_agent`)

Each endpoint analysis is implemented independently while adhering to a
shared structure and reporting workflow.

---

## Structure

* `projects/`

  * Active and completed clinical trial analyses and agent systems
  * Each project is self-contained and reproducible

* `framework/`

  * Shared R utilities used across projects
  * Includes standardized path helpers and reusable functions

---

## Endpoint Mapping

* Binary endpoints → logistic regression–based analyses
* Time-to-event endpoints → Kaplan–Meier estimation and Cox proportional 
hazards modeling
* Longitudinal endpoints → repeated-measures and mixed-effects modeling

---

## Clinical Study Report (CSR) Alignment

The analyses in this project are organized to reflect common Clinical Study
Report structures:

* Baseline characteristics and population summaries
* Primary endpoint analysis
* Secondary and exploratory analyses
* Model diagnostics and sensitivity analyses

Tables, Figures, and Listings (TFLs) are structured to align with typical CSR
presentation workflows.

---

## Agent Outputs (CT05)

The TTE Method Agent produces structured outputs in:

```text
ct05_tte_methods_agent/results/
├── reports/
├── summary/
```

### Outputs include:

* Case-level markdown reports
* Batch summary report across scenarios
* Structured decision logic outputs (JSON logs excluded from version control)

---

## How to start a new project

1. Copy `biostatistics-project-template/` into `projects/`
2. Rename it (e.g., `ct02_time_to_event`)
3. Add raw data to `data/raw/`
4. Run scripts in `src/` from `00` → `07`
5. Render reports from `rmd/`

---

## Conventions

* Raw data is never modified
* All analysis uses `.rds` files from `data/processed/`
* Primary analyses are prespecified
* Exploratory analyses are clearly labeled

---

## Reproducibility

This project is structured as a reproducible statistical analysis workflow.

The analysis environment is managed using `renv` to ensure consistent package
versions and reproducible results across systems.

To reproduce the analysis:

1. Open `clinical-trials.Rproj` in RStudio
2. Run `renv::restore()` to install required packages
3. Render analyses using Quarto (`quarto render`) or the RStudio Render button

All analysis outputs are generated from source code and are not stored as 
primary artifacts.

---

## Key Evolution

> This workspace extends beyond traditional statistical analysis by introducing
agent-based decision systems that operationalize biostatistical reasoning into
reproducible, structured workflows.
