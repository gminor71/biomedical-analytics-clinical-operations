# Master Trial 01 — Starter Data Dictionary

**Trial:** Oncology Immunotherapy Simulation

---

## Purpose

This document defines the foundational datasets used across the master trial simulation.

The goal is to maintain consistent naming conventions and relationships between operational data domains while keeping datasets simple and operationally realistic.

This is **not** intended to replicate CDISC/SDTM standards.

---

## Core Principles

* One row = one logical observation
* Subject ID is the primary linking field across domains
* Site ID supports risk and monitoring comparisons
* Dates reflect operational timing and compliance patterns

---

# 1. Subjects Dataset

**File:** `subjects.csv`
**Purpose:** Subject-level enrollment and baseline information.

| Column                      | Description                           |
| --------------------------- | ------------------------------------- |
| subject_id                  | Unique subject identifier             |
| site_id                     | Site where subject enrolled           |
| consent_date                | Date informed consent signed          |
| enrollment_date             | Date subject officially enrolled      |
| sex                         | Biological sex                        |
| age                         | Age at enrollment                     |
| ecog                        | ECOG performance status               |
| tumor_type                  | Simplified tumor category             |
| baseline_measurable_disease | Yes/No flag                           |
| status                      | Active / Off treatment / Discontinued |

---

# 2. Visits Dataset

**File:** `visits.csv`
**Purpose:** Track protocol visit compliance.

| Column              | Description                   |
| ------------------- | ----------------------------- |
| subject_id          | Link to subject               |
| site_id             | Site identifier               |
| visit_name          | Screening / Cycle1Day1 / etc  |
| planned_date        | Protocol scheduled visit date |
| actual_date         | Actual visit date             |
| visit_window_status | On-time / Early / Late        |
| visit_completed     | Yes/No                        |

---

# 3. Laboratory Results Dataset

**File:** `labs.csv`
**Purpose:** Key safety labs influencing treatment decisions.

| Column                 | Description                     |
| ---------------------- | ------------------------------- |
| subject_id             | Link to subject                 |
| site_id                | Site identifier                 |
| visit_name             | Associated visit                |
| lab_date               | Date collected                  |
| lab_test               | ALT, AST, Creatinine, etc       |
| lab_value              | Numeric result                  |
| lab_unit               | Measurement unit                |
| grade                  | Simplified toxicity grade (0–4) |
| clinically_significant | Yes/No                          |

---

# 4. Adverse Events Dataset

**File:** `ae_log.csv`
**Purpose:** Capture treatment-emergent adverse events.

| Column          | Description                     |
| --------------- | ------------------------------- |
| subject_id      | Link to subject                 |
| site_id         | Site identifier                 |
| ae_term         | Event description               |
| onset_date      | Event start date                |
| end_date        | Event end date                  |
| grade           | Severity grade                  |
| serious_flag    | Yes/No                          |
| related_to_drug | Yes/No                          |
| action_taken    | None / Dose hold / Discontinued |
| report_date     | Date entered or reported        |

---

# 5. Dose Administration Dataset

**File:** `dose_admin.csv`
**Purpose:** Track treatment exposure and modifications.

| Column      | Description                      |
| ----------- | -------------------------------- |
| subject_id  | Link to subject                  |
| site_id     | Site identifier                  |
| cycle       | Treatment cycle number           |
| dose_date   | Administration date              |
| dose_status | Given / Delayed / Held / Reduced |
| reason      | AE / Lab abnormality / Other     |
| notes       | Optional comments                |

---

# 6. Imaging Dataset

**File:** `imaging.csv`
**Purpose:** Oncology efficacy assessment tracking.

| Column           | Description            |
| ---------------- | ---------------------- |
| subject_id       | Link to subject        |
| site_id          | Site identifier        |
| scan_date        | Imaging date           |
| scheduled_window | On-time / Early / Late |
| response         | CR / PR / SD / PD      |
| progression_flag | Yes/No                 |

---

# 7. Query Tracking Dataset

**File:** `queries.csv`
**Purpose:** Operational query management and data quality monitoring.

| Column        | Description                            |
| ------------- | -------------------------------------- |
| query_id      | Unique query identifier                |
| subject_id    | Related subject                        |
| site_id       | Site identifier                        |
| domain        | Visits / Labs / AE / Dose / Imaging    |
| issue_type    | Missing / Inconsistent / Clarification |
| opened_date   | Date query opened                      |
| resolved_date | Date resolved                          |
| status        | Open / Answered / Closed               |
| aging_days    | Days open                              |

---

# Site-Level Metrics (Derived)

These are not direct datasets but may be generated:

* Enrollment counts by site
* Query aging summaries
* AE reporting timeliness
* Protocol deviation counts

Used for risk-based monitoring scenarios.

---

## Notes

* Additional columns may be added as scenarios evolve.
* Structural consistency should be maintained across all timeline phases.
* Data should prioritize operational realism over complexity.

---
## Subject ID Convention

Format: SSS-XXX

SSS = Site identifier (3 digits)
XXX = Sequential subject number within site
Example: 003-012 = Site 003, subject 12