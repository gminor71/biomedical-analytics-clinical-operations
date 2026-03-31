
# CT08 – Continuous Endpoints Agent

## Overview

**CT08 – Continuous Endpoints Agent** is a Python-based decision engine that
translates biostatistical guidance into structured, executable logic
for **continuous endpoint** analysis in clinical trials.

The system evaluates input scenarios and produces:

* Method recommendations
* Effect measure guidance
* Structured outputs for both human review and downstream use

---

## Purpose

This project demonstrates how clinical and statistical reasoning can be
transformed into:

* Rule-based decision systems
* Reproducible analytical workflows
* Early-stage agentic AI design patterns

Rather than performing statistical modeling directly, the agent focuses on:

> **“Given the study conditions, what is the appropriate analytical approach for a continuous endpoint?”**

---

## Relationship to the Repository

This project is part of a layered system:

| Layer     | Location                       | Role                                   |
| --------- | ------------------------------ | -------------------------------------- |
| Knowledge | `methods-library/`             | Statistical methods and interpretation |
| Logic     | `methods-library/agent-logic/` | Decision rules and frameworks          |
| Execution | `ct08_continuous_endpoints_agent/` | Implemented agent system           |

CT08 operationalizes:

* Continuous endpoint method selection
* Effect measure guidance
* Agent-based decision logic

---

## Version 1 Scope

### Included

* Continuous endpoint support
* Rule-based method selection
* Effect measure guidance
* YAML-based structured input
* Markdown report generation
* JSON run logging
* Batch processing capability
* Batch summary reporting

### Not Included

* Model fitting or statistical computation
* Automated diagnostics
* Repeated-measures or longitudinal modeling
* Multi-endpoint routing
* LLM or AI-generated interpretation
* Integration with prior CT project outputs

---

## Project Structure

```text
ct08_continuous_endpoints_agent/
├── run_agent.py
├── run_agent_batch.py
├── data/
├── results/
├── src/
```

## Input

The agent reads structured YAML files from:

```text
data/processed/
```

Each file represents a single analysis scenario.

### Example fields

* `endpoint_type`
* `endpoint_name`
* `n_groups`
* `sample_size`
* `normality`
* `variance_homogeneity`
* `covariates`

---

## Output

The agent writes outputs to the standard `results/` structure.

### Structure

```text
results/
├── reports/
├── logs/
└── summary/
```


### Reports

Markdown reports generated per case:

```text
results/reports/<analysis_id>_report.md
```

---

### Logs

Structured JSON run logs:

```text
results/logs/<analysis_id>_run.json
```

---

### Batch Summary

Generated in batch mode:

```text
results/summary/batch_summary.md
```

This file provides:

* Aggregated overview of all processed cases
* Comparison of PH status and warning signals
* Consolidated warning review

## Effect Measures Reference

For interpretation of continuous endpoint effect measures used in 
this project, see:

- `methods-library/effect-size/continuous_effect_measures.md`

---

## How to Run

### Single Case Mode

```bash
python run_agent.py
```

Processes a single YAML input file and generates:

* One markdown report
* One JSON log

---

### Batch Mode

```bash
python run_agent_batch.py
```

Processes all YAML files in:

```text
data/processed/
```

Generates:

* Individual reports for each case
* Individual JSON logs
* One batch summary report

---

## Execution Modes

The project supports two execution modes:

| Mode       | Script               | Purpose                                                  |
| ---------- | -------------------- | -------------------------------------------------------- |
| Single Run | `run_agent.py`       | Testing and debugging individual cases                   |
| Batch Run  | `run_agent_batch.py` | Processing multiple cases and generating summary outputs |

---

## Agent Prompt Logic

This agent follows a structured, template-driven approach to statistical 
decision support.  

The design aligns with the standard defined in:

`methods-library/agent-logic/agent_prompt_template.md`

---

### 🧠 Role

The agent operates as a:

> Clinical trial biostatistician specializing in continuous endpoint analysis

---

### 🎯 Task

Evaluate a structured continuous endpoint analysis case and:

- Recommend an appropriate primary method
- Identify an appropriate effect measure
- Suggest alternative methods when assumptions are not fully supported
- Flag risks related to non-normality and unequal variance
- Provide structured notes for interpretation

---

### 📊 Context (Input)

The agent consumes structured input from YAML files, including:

- `study_id`
- `analysis_id`
- `endpoint_type`
- `endpoint_name`
- `n_groups`
- `sample_size`
- `normality`
- `variance_homogeneity`
- `covariates`

These inputs define the analysis scenario and drive decision logic.

---

### 🔧 Instructions (Evaluation Logic)

The agent follows a deterministic, stepwise evaluation:

1. **Confirm endpoint type**
   - Validate that the endpoint is continuous

2. **Assign baseline recommendation**
   - Independent t-test for two groups
   - ANOVA for more than two groups

3. **Evaluate normality**
   - If non-normal, suggest nonparametric alternatives

4. **Evaluate variance assumptions**
   - If variance is unequal for two groups, prefer Welch t-test

5. **Evaluate covariate adjustment**
   - If covariates are present, suggest linear regression

---

### 📦 Output Format

The agent returns a structured decision object:


```json
{
  "study_id": "",
  "analysis_id": "",
  "endpoint_type": "",
  "endpoint_name": "",
  "primary_method": "",
  "effect_measure": "",
  "alternative_methods": [],
  "warnings": [],
  "notes": []
}
```

### ⚠️ Quality Controls

The agent enforces the following safeguards:

- Rejects unsupported endpoint types
- Flags non-normality concerns
- Identifies unequal variance issues
- Suggests alternative adjusted methods when covariates are present
- Returns structured warnings instead of silent failures

## Design Principles

* **Deterministic first**
  Rule-based, explainable decision logic

* **Modular structure**
  Separation of input, logic, orchestration, and reporting

* **Auditable outputs**
  Markdown + JSON for traceability

* **Scalable architecture**
  Designed to support expansion into multi-endpoint and AI-assisted systems
  
---

## Key Principle

> This project demonstrates how continuous endpoint method selection can be 
translated into structured decision systems, 
forming the foundation for agentic AI in clinical research.
