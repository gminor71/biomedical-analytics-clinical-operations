# Master Trial 01 — Oncology Immunotherapy Simulation

---

## Purpose

Master Trial 01 is a scenario-driven clinical operations simulation designed to model realistic clinical trial execution within an oncology immunotherapy setting.

This trial serves as a training environment for developing:

* Clinical Research Associate (CRA) monitoring skills
* Site oversight and risk assessment
* Query and deviation management
* Safety monitoring workflows
* Operational decision-making across an evolving trial timeline

The simulation supports the broader biomedical-analytics-clinical-operations program by connecting clinical operations practice with downstream biostatistical analysis and analytics.

---

## Trial Overview

* **Phase:** II
* **Design:** Open-label, multicenter oncology immunotherapy trial
* **Primary Endpoint:** Objective Response Rate (ORR)
* **Estimated Enrollment:** ~120 subjects
* **Sites:** Multiple simulated clinical sites with varying operational characteristics

This trial is intentionally simplified for training purposes while maintaining operational realism.

Detailed protocol information is located in:

```
trial_context/
```

---

## Simulation Philosophy

This master trial follows a **scenario-driven, evolving timeline** model.

Key principles:

* The trial exists as a continuous operational environment.
* Scenarios represent time-based phases rather than independent cases.
* Not all findings represent problems — routine monitoring and normal variability are expected.
* Risk is assessed categorically (Low / Medium / High) based on operational trends.

The goal is to practice realistic judgment rather than scripted problem-solving.

---

## CRA Perspective

Simulation activities are performed from the perspective of a CRA assigned primarily to:

```
Site 003 — High Enrolling Site
```

The CRA maintains visibility into trial-wide trends through central monitoring metrics while focusing reporting and follow-up activities on the assigned site.

---

## Timeline Model

The simulation progresses through milestone-based phases:

```
t0_startup_context
t1_early_enrollment
t2_active_monitoring
t3_risk_escalation
t4_pre_database_lock
```

Each timeline folder introduces new data, operational context, and monitoring decisions.

---

## Folder Structure Overview

### trial_context/

Stable reference materials describing the protocol and study design.

---

### sites/

Site-level folders containing site profiles and local operational context.

---

### timeline/

Scenario engine driving simulation progression over time.

---

### data/

Operationally realistic datasets used throughout scenarios.

---

### central_monitoring/

Trial-level metrics used for cross-site comparison and risk evaluation.

---

### outputs/

CRA-generated work products such as:

* Monitoring Visit Reports
* Follow-Up Letters
* Risk Assessments
* Scenario summaries

---

## Relationship to the Overall Program

This master trial connects clinical operations training with analytical work:

```
Clinical Trials Operations System (reference)
        ↓
Master Trial 01 Simulation (execution)
        ↓
Clinical Trial Statistical Analysis (CT projects)
        ↓
AI and Advanced Analytics
```

Operational decisions made within the simulation may influence downstream analytical exercises.

---

## Working Guidance

* Trial context files are generally stable references.
* Timeline folders evolve as scenarios progress.
* Outputs represent work generated during simulation activities.
* Templates and data dictionaries are maintained outside this folder for consistency and reuse.

---

## Maintainer Note

This trial is intentionally designed as a learning and systems-thinking environment.

Build incrementally:

1. Establish baseline trial state
2. Run scenarios through timeline phases
3. Generate monitoring outputs
4. Observe how operational decisions affect data quality and analysis readiness
