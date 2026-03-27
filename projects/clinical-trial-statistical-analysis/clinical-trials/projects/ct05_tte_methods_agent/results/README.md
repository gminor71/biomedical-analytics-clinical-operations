# Results – CT05 TTE Method Agent

## Overview

This directory contains all generated outputs from the **CT05 TTE Method Agent**.

Outputs are organized to support both:

* **case-level review**
* **batch-level summary analysis**

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
* Primary method recommendation
* Proportional hazards (PH) assumption status
* Alternative method suggestions
* Warnings
* Notes

### Example

```text
tte_case_001_report.md
tte_case_002_report.md
```

---

## Logs

📁 `results/logs/`

Structured JSON outputs for each run.

### Purpose

* Preserve full decision object
* Enable debugging and validation
* Support future automation and integration

### Example

```text
tte_case_001_run.json
tte_case_002_run.json
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
* Table of key attributes:

  * Analysis ID
  * Endpoint
  * Run status
  * PH status
  * Primary method
  * Warning count
* Detailed warning breakdown by case

---

## Execution Modes

### Single Case

```bash
python run_agent.py
```

Generates:

* One report
* One JSON log

---

### Batch Mode

```bash
python run_agent_batch.py
```

Generates:

* Multiple reports
* Multiple logs
* One batch summary

---

## Key Principle

> Results are structured to support both **traceability (per-case outputs)** and **comparative analysis (batch summary)**.

---
