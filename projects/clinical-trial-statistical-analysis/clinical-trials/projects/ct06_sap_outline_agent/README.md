# CT06 – SAP Outline Agent

## Overview

**CT06 – SAP Outline Agent** is a Python-based decision system that 
translates clinical trial design inputs into a structured, first-pass 
**Statistical Analysis Plan (SAP) outline**.

The system evaluates input scenarios and produces:

* Draft SAP analysis components
* Assumption-aware planning considerations
* Structured outputs for both human review and downstream use

---

## Purpose

This project demonstrates how clinical and statistical planning can be 
transformed into:

* Rule-based planning systems
* Reproducible analysis design workflows
* Early-stage agentic AI design patterns

Rather than performing statistical modeling directly, the agent focuses on:

> **“Given the study design and endpoint, what should the SAP include?”**

---

## Relationship to the Repository

This project is part of a layered system:

| Layer     | Location                       | Role                                   |
|-----------|--------------------------------|----------------------------------------|
| Knowledge | `methods-library/`             | Statistical methods and interpretation |
| Logic     | `methods-library/agent-logic/` | Decision rules and frameworks          |
| Execution | `ct06_sap_outline_agent/`      | Implemented agent system               |

CT06 operationalizes:

* SAP outline development
* Estimand framing
* Analysis population definition
* Early-stage statistical planning logic

---

## Version 1 Scope

### Included

* Time-to-event SAP outline support
* Rule-based SAP drafting logic
* Estimand-aware planning structure
* YAML-based structured input
* Markdown report generation
* JSON run logging
* Batch processing capability
* Batch summary reporting

### Not Included

* Full SAP document generation
* Statistical modeling or computation
* Automated assumption diagnostics
* Multi-endpoint routing
* LLM-generated narrative writing
* Integration with prior CT project outputs

---

## Project Structure

```text
ct06_sap_outline_agent/
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

Each file represents a single SAP planning scenario.

### Example fields

* `endpoint_type`
* `endpoint_name`
* `objective`
* `estimand_strategy`
* `analysis_population`
* `covariates`
* `stratification_factors`
* `missing_data_risk`
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

Each report contains a **draft SAP outline** including:

* Objective
* Estimand
* Analysis population
* Endpoint definition
* Primary method
* Covariate strategy
* Stratification
* Missing data handling
* Sensitivity analyses
* Follow-up items
* Warnings and notes

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
* Comparison of planning decisions across scenarios
* Consolidated review of risks and follow-up gaps

---

## How to Run

### Single Case Mode

```bash
python run_agent.py
```

### Batch Mode

```bash
python run_agent_batch.py
```
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

> Clinical trial biostatistician responsible for drafting SAP outlines

---

### 🎯 Task
Convert a structured clinical trial scenario into a draft SAP outline by:

- Defining the analysis objective
- Framing the estimand
- Identifying the analysis population
- Recommending a primary analysis method
- Identifying covariates and stratification factors
- Highlighting sensitivity analyses
- Flagging missing or incomplete planning elements

---

### 📊 Context (Input)

The agent consumes structured input from YAML files, including:

- `study_id`
- `analysis_id`
- `endpoint_type`
- `endpoint_name`
- `objective`
- `estimand_strategy`
- `analysis_population`
- `covariates`
- `stratification_factors`
- `intercurrent_events`
- `missing_data_risk`
- `censoring_concern`
- `ph_assumption_status`

---

### 🔧 Instructions (Evaluation Logic)

The agent follows a deterministic, stepwise evaluation:

1. **Confirm endpoint type**
   - Validate that the endpoint is time-to-event (v1 scope)
2. **Draft objective**
  - Use provided objective or apply default phrasing
3. **Define estimand**
  - Use provided strategy or flag missing components
4. **Assign analysis population**
  - Use input or flag missing specification
5. **Define endpoint**
  - Standardize endpoint description
6. **Assign primary method**
  - Kaplan-Meier + Log-rank + Cox PH (for TTE)
7. **Evaluate covariates and stratification**
  - Include provided inputs or note absence
8. **Assess missing data and censoring**
  - Flag risks and assumptions
9. **Recommend sensitivity analyses**
  - PH diagnostics
  - RMST or alternative models if needed
  - Censoring sensitivity if applicable
10. **Identify follow-up items**
  - Missing estimand components
  - Missing intercurrent event handling
  - Incomplete planning inputs

---

### 📦 Output Format

The agent returns a structured SAP outline:

```json
{
  "study_id": "",
  "analysis_id": "",
  "endpoint_type": "",
  "endpoint_name": "",
  "sap_outline": {
    "objective": "",
    "estimand": "",
    "analysis_population": "",
    "endpoint_definition": "",
    "primary_method": "",
    "covariate_strategy": "",
    "stratification": "",
    "missing_data_handling": "",
    "sensitivity_analyses": [],
    "follow_up_items": []
  },
  "warnings": [],
  "notes": []
}
```

### ⚠️ Quality Controls

The agent enforces the following safeguards:

- Rejects unsupported endpoint types
- Flags missing or incomplete SAP components
- Identifies assumptions requiring validation
- Highlights potential bias sources
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

> This project demonstrates how biostatistical planning can be translated 
into structured decision systems, forming a bridge between study design 
and statistical analysis.

