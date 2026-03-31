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

## Agent Prompt Logic

This agent follows a structured, template-driven approach to statistical decision support.  
The design aligns with the standard defined in:

`methods-library/agent-logic/agent_prompt_template.md`

---

### 🧠 Role

The agent operates as a:

> Clinical trial biostatistician specializing in time-to-event (TTE) analysis

---

### 🎯 Task

Evaluate a structured time-to-event analysis case and:

- Recommend an appropriate primary method
- Assess the proportional hazards (PH) assumption
- Suggest alternative methods when assumptions are violated
- Flag risks related to sample size, event rate, and censoring
- Provide structured notes for interpretation

---

### 📊 Context (Input)

The agent consumes structured input from YAML files, including:

- `study_id`
- `analysis_id`
- `endpoint_type`
- `endpoint_name`
- `ph_assumption_status`
- `sample_size`
- `event_rate`
- `censoring_concern`

These inputs define the analysis scenario and drive decision logic.

---

### 🔧 Instructions (Evaluation Logic)

The agent follows a deterministic, stepwise evaluation:

1. **Confirm endpoint type**
   - Validate that the endpoint is time-to-event

2. **Assign baseline recommendation**
   - Kaplan-Meier estimation
   - Log-rank test
   - Cox proportional hazards model

3. **Evaluate PH assumption**
   - If satisfied → proceed with Cox PH
   - If violated → recommend alternatives:
     - Stratified Cox model
     - Time-varying covariates
     - Restricted Mean Survival Time (RMST)
     - Accelerated Failure Time (AFT) model
   - If unknown → flag need for diagnostics

4. **Assess sample size and event rate**
   - Flag small sample size (<50)
   - Flag low event rate (<20%)

5. **Assess censoring**
   - Flag potential informative censoring

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
  "ph_assumption_status": "",
  "alternative_methods": [],
  "warnings": [],
  "notes": []
}


```

### ⚠️ Quality Controls

The agent enforces the following safeguards:

- Rejects unsupported endpoint types
- Flags violations or uncertainty in the proportional hazards assumption
- Identifies data limitations (sample size, event rate)
- Highlights potential bias due to informative censoring
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

> This project demonstrates how biostatistical knowledge 
can be translated into structured decision systems, forming the foundation
for agentic AI in clinical research.
