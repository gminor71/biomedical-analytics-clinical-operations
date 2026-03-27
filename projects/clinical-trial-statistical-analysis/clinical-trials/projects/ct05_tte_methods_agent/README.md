# CT05 – TTE Method Agent

## Overview

**CT05 – TTE Method Agent** is a Python-based decision engine that 
translates biostatistical guidance into structured, executable logic 
for **time-to-event (TTE)** endpoint analysis in clinical trials.

The system evaluates input scenarios and produces:

* Method recommendations
* Assumption-aware adjustments
* Structured outputs for both human review and downstream use

---

## Purpose

This project demonstrates how clinical and statistical reasoning can be 
transformed into:

* Rule-based decision systems
* Reproducible analytical workflows
* Early-stage agentic AI frameworks

Rather than performing statistical modeling directly, the agent focuses on:

> **“Given the study conditions, what is the appropriate analytical approach?”**

---

## Relationship to the Repository

This project is part of a layered system:

| Layer     | Location                       | Role                                   |
| --------- | ------------------------------ | -------------------------------------- |
| Knowledge | `methods-library/`             | Statistical methods and interpretation |
| Logic     | `methods-library/agent-logic/` | Decision rules and frameworks          |
| Execution | `ct05_tte_method_agent/`       | Implemented agent system               |

CT05 operationalizes:

* Time-to-event method selection
* Cox proportional hazards assumptions
* Agent-based decision logic

---

## Version 1 Scope

### Included

* Time-to-event endpoint support
* Rule-based method selection
* PH assumption-aware recommendations
* YAML-based structured input
* Markdown report generation
* JSON run logging
* Batch processing capability
* Batch summary reporting

### Not Included

* Model fitting or statistical computation
* Automated assumption diagnostics
* Multi-endpoint routing
* LLM or AI-generated interpretation
* Integration with prior CT project outputs

---

## Project Structure

```text
ct05_tte_method_agent/
├── run_agent.py
├── run_agent_batch.py
├── data/
├── results/
├── src/
```

---

## Input

The agent reads structured YAML files from:

```text
data/processed/
```

Each file represents a single analysis scenario.

### Example fields

* `endpoint_type`
* `sample_size`
* `event_rate`
* `ph_assumption_status`
* `censoring_concern`

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

---

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

## Core Logic

The agent follows a structured decision pathway:

```text
Input
→ Validate endpoint type
→ Assign baseline TTE method
→ Evaluate PH assumption
→ Apply warnings and alternative methods
→ Generate outputs
```

### Default Framework

* Kaplan-Meier estimation
* Log-rank comparison
* Cox proportional hazards regression

### When PH is violated

The agent may recommend:

* Stratified Cox model
* Time-varying covariates
* Restricted Mean Survival Time (RMST)
* Accelerated Failure Time (AFT) model

---

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

> This project demonstrates how biostatistical knowledge 
can be translated into structured decision systems, forming the foundation
for agentic AI in clinical research.
