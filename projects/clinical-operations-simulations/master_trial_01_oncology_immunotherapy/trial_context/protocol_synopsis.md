# Master Trial 01 — Oncology Immunotherapy Trial (Simulation Synopsis)

## Trial Title

Phase II Multicenter Study Evaluating an Investigational Immunotherapy in Patients With Advanced Solid Tumors

---

## Study Purpose

This simulation trial is designed to model realistic clinical trial execution within an oncology setting and support training in:

* Clinical research associate (CRA) monitoring workflows
* Site oversight and risk identification
* Data quality and query management
* Safety monitoring and escalation processes
* Operational decisions influencing downstream analysis

The study serves as a training environment within the biomedical-analytics-clinical-operations program.

---

## Study Design

* **Phase:** II
* **Design:** Open-label, single-arm immunotherapy trial
* **Setting:** Multicenter (U.S.-based simulated sites)
* **Population:** Adults with advanced or metastatic solid tumors
* **Estimated Enrollment:** ~120 subjects
* **Primary Analysis Population:** Intent-to-Treat (ITT)

---

## Investigational Treatment

* Intravenous immunotherapy administered every 3 weeks (Q3W)
* Treatment cycles continue until:

  * disease progression
  * unacceptable toxicity
  * withdrawal of consent

Dose delays or discontinuation may occur due to adverse events or laboratory abnormalities.

---

## Primary Objective

Evaluate anti-tumor activity of the investigational immunotherapy.

### Primary Endpoint

* Objective Response Rate (ORR) based on investigator assessment using RECIST-style response criteria.

---

## Secondary Objectives

* Safety and tolerability
* Duration of response (DoR)
* Progression-free survival (PFS)
* Time to response

---

## Key Inclusion Criteria (Simplified)

* Age ≥ 18 years
* Histologically confirmed advanced solid tumor
* ECOG performance status 0–1
* Adequate organ function based on laboratory thresholds
* Measurable disease at baseline imaging

---

## Key Exclusion Criteria (Simplified)

* Active autoimmune disease requiring systemic therapy
* Prior exposure to similar immunotherapy class
* Uncontrolled CNS metastases
* Recent systemic anticancer therapy within protocol-defined washout window

---

## Visit Schedule (Simplified)

### Screening

* Informed consent
* Eligibility confirmation
* Baseline labs and imaging

### Treatment Phase (Q3W Cycles)

* Physical exam
* Safety labs
* AE assessment
* Dose administration

### Imaging Assessments

* Baseline
* Every 6 weeks during initial treatment period
* Every 9–12 weeks thereafter

---

## Safety Monitoring

Safety monitoring is central to this trial simulation.

Key operational areas include:

* Treatment-emergent adverse events (TEAEs)
* Serious adverse event (SAE) reporting timelines
* Immune-related adverse events (irAEs)
* Dose delays and discontinuations
* Laboratory-driven treatment decisions

Sites are expected to maintain timely documentation and data entry consistent with protocol expectations.

---

## Operational Focus Areas (CRA Training)

This master trial is designed to support scenario-based monitoring activities such as:

* Eligibility verification and protocol compliance
* Delegation log oversight
* AE/SAE timeliness and reconciliation
* Dose modifications linked to safety findings
* Imaging schedule adherence
* Query generation and resolution tracking
* Risk-based monitoring escalation decisions

---

## Data Domains (Simulation Scope)

Operationally realistic datasets may include:

* Subject-level enrollment data
* Visit schedule and compliance
* Laboratory results
* Adverse event logs
* Dose administration records
* Imaging assessments
* Query tracking logs

Datasets are intentionally simplified for training use while maintaining realistic workflow relationships.

---

## Planned Simulation Timeline

The master trial will evolve through scenario phases:

1. Startup and Site Initiation
2. Early Enrollment and Eligibility Review
3. Active Monitoring and Safety Oversight
4. Mid-Study Risk Escalation
5. Pre-Database Lock Data Review

Each scenario represents a progression in operational complexity.

---

## Program Integration

This master trial supports the broader program architecture:

```
Clinical Trials Operations System (reference)
        ↓
Master Trial Oncology Simulation (execution)
        ↓
Clinical Trial Statistical Analysis (CT projects)
        ↓
AI and Advanced Analytics
```

Operational decisions made during simulation scenarios may later inform downstream analytical exercises.
