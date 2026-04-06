# Project Management and Delivery Framework

This directory defines a structured, decision-driven project management framework supporting
clinical research operations, biostatistics, and analytics workflows.

The framework is designed for regulated environments where documentation,
traceability, and cross-functional coordination are critical.

It combines standardized templates with automation and agent-based logic to support
consistent project setup, execution, and tracking.

---

## Framework Structure

- templates/  
  Standardized artifacts used during project execution

- guides/  
  Reference documents describing how to apply project management practices
  across clinical and analytics workflows

- examples/  
  Applied examples demonstrating how project management artifacts are used
  in clinical trial and biostatistics contexts

- logic/  
  Agent-based logic modules that recommend, generate, and update project artifacts

- Index.md  
  Central index of all templates, guides, and logic modules

---

## Core Capabilities

### 1. Project Setup (PM01)
- Recommends required and optional project artifacts based on project profile
- Outputs structured configuration for project execution
- Links directly to standardized templates

### 2. Artifact Generation
- Automatically generates project workspaces
- Supports both:
  - Markdown (documentation/reference)
  - Excel (operational tracking)

### 3. Operational Triage (PM02)
- Classifies items as Risk, Issue, Action, or Decision
- Assigns priority, ownership, and escalation flags
- Produces structured triage outputs with rationale

### 4. RAID Log Integration
- Automatically appends triaged items to working RAID logs
- Maintains consistent structure across projects
- Reduces manual entry and improves traceability

---

## Example Workflow

```text
Project Profile YAML
   ↓
PM01 → Artifact Recommendations
   ↓
Workspace Generation (MD + Excel)
   ↓
Operational Event
   ↓
PM02 → Triage Output
   ↓
Append to RAID Log (Excel)
```

## Scope and Positioning

This framework serves as the **operational layer connecting**:
- Clinical trial operations (CRA workflows)
- Biostatistical analysis and reporting
- Data and analytics processes

It is intended to be used alongside:
- clinical-trials-operations-system/
- clinical-trials/
- methods-library/

---

## Design Principles

- Structured but lightweight
- Aligned with regulated clinical environments
- Focused on practical outputs, not theory
- Traceable across lifecycle phases
- Compatible with reproducible analytics workflows
- Automation-enabled to reduce manual overhead
---

## Key Outputs

- Project Charter
- RAID Log (Risk, Action, Issue, Decision)
- Decision Log
- Status Reports
- Deliverables Tracker
- Communication Plan

---

## Relationship to Clinical Operations Binder

This framework defines the **core project management structure**.

The Clinical Trial Operations Binder applies this framework
from a CRA and site management perspective, incorporating monitoring,
site communication, and inspection readiness workflows..


## Summary

This module transforms project management from a static template-based approach
into a structured, reproducible, and automation-supported system aligned with
clinical research and analytics delivery.
