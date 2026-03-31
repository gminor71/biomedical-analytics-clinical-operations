
# CT07 – Binary Method Agent

## Overview

**CT07 – Binary Method Agent** is a Python-based decision engine that
translates biostatistical guidance into structured, executable logic
for **binary endpoint** analysis in clinical trials.

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

> **“Given the study conditions, what is the appropriate analytical approach for a binary endpoint?”**

---

## Relationship to the Repository

This project is part of a layered system:

| Layer     | Location                       | Role                                   |
| --------- | ------------------------------ | -------------------------------------- |
| Knowledge | `methods-library/`             | Statistical methods and interpretation |
| Logic     | `methods-library/agent-logic/` | Decision rules and frameworks          |
| Execution | `ct07_binary_methods_agent/`   | Implemented agent system               |

CT07 operationalizes:

* Binary endpoint method selection
* Effect measure guidance
* Agent-based decision logic

---

## Version 1 Scope

### Included

* Binary endpoint support
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
* Multi-endpoint routing
* LLM or AI-generated interpretation
* Integration with prior CT project outputs

---

## Project Structure

```text
ct07_binary_methods_agent/
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
* `sample_size`
* `event_rate`
* `expected_cell_count`
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

---

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

> Clinical trial biostatistician specializing in binary endpoint analysis

---

### 🎯 Task

Evaluate a structured binary endpoint analysis case and:

- Recommend an appropriate primary method
- Identify an appropriate effect measure
- Suggest alternative methods when covariate adjustment is relevant
- Flag risks related to sample size, event rate, and censoring
- Provide structured notes for interpretation

---

### 📊 Context (Input)

The agent consumes structured input from YAML files, including:

- `study_id`
- `analysis_id`
- `endpoint_type`
- `endpoint_name`
- `sample_size`
- `event_rate`
- `expected_cell_count`
- `covariates`

These inputs define the analysis scenario and drive decision logic.

---

### 🔧 Instructions (Evaluation Logic)

The agent follows a deterministic, stepwise evaluation:

1. **Confirm endpoint type**
   - Validate that the endpoint is binary

2. **Assign baseline recommendation**
   - Chi-square test
   - Risk difference / Risk ratio

3. **Evaluate sample size and expected cell counts**
   - If sample size is small or expected counts are low, 
   recommend Fisher's exact test

4. **Evaluate covariate adjustment**
   - If covariates are present, suggest logistic regression

5. **Evaluate rare event risk**
   - If event rate is very low, flag possible instability and 
   note alternative exact or penalized approaches if needed

---

### 📦 Output Format

The agent returns a structured decision object:

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
- Flags small sample size or sparse data concerns
- Identifies rare event instability risks
- Suggests alternative adjusted methods when covariates are present
- Returns structured warnings instead of silent failures

```

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

> This project demonstrates how binary endpoint method selection can be 
translated into structured decision systems, forming the foundation for 
agentic AI in clinical research.
