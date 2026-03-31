# Results – CT06 SAP Outline Agent

## Overview

This directory contains all generated outputs from the **CT06 SAP Outline Agent**.

Outputs are organized to support both:

* **case-level SAP planning review**
* **batch-level comparison across scenarios**

---

## Structure

```text
results/
├── reports/
├── logs/
└── summary/
```


---

## Reports

📁 `results/reports/`

Markdown reports generated for each individual case.

### Contents

Each report includes:

* Run summary (study, analysis ID, endpoint)
* Draft SAP outline:
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
* Warnings
* Notes

### Example

```text
sap_case_001_report.md
sap_case_002_report.md
```


---

## Logs

📁 `results/logs/`

Structured JSON outputs for each run.

### Purpose

* Preserve full SAP outline structure
* Enable debugging and validation
* Support future automation and integration

### Example


```text
sap_case_001_run.json
sap_case_002_run.json
```

---

## Summary

📁 `results/summary/`

Batch-level summary of all processed cases.

### File

```text
batch_summary.md
```


### Contents

* Total cases processed
* Run status summary
* Per-case overview including:
  * Primary method
  * Sensitivity analyses
  * Follow-up items
  * Warnings
* Consolidated review of:
  * sensitivity patterns
  * follow-up gaps
  * potential risk signals

---

## Execution Modes

### Single Case

```bash
python run_agent.py
```

Generates:

* Multiple reports
* Multiple logs
* One batch summary

---

## Key Principle

> Results are structured to support both **traceability (per-case outputs)** and **structured SAP planning review across scenarios**.

---