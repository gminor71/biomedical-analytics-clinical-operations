# Results – CT07 Binary Method Agent

## Overview

This directory contains all generated outputs from the **CT07 Binary Method Agent**.

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
* Effect measure guidance
* Alternative method suggestion
* Warnings
* Notes

### Example

```text
binary_case_001_report.md
binary_case_002_report.md
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
binary_case_001_run.json
binary_case_002_run.json
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
  * Effect measure
  * Alternative methods
  * Warnings
  * Notes

---

## Execution Modes

### Single Case

```bash
python run_agent.py
```

Generates:

* One report
* One JSON log

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

> Results are structured to support both **traceability (per-case outputs)**
and **comparative review across binary endpoint scenarios**.

---