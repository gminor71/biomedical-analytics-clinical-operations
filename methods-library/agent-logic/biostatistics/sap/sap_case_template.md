# SAP Case Template

## Overview

This document defines the standard input structure for the **CT06 SAP Outline Agent**.

The template is designed to support structured SAP planning across endpoint types by
standardizing how analysis scenarios are described in YAML format.

Current supported endpoint types:

- `time_to_event`
- `binary`
- `continuous`

---

## Purpose

The SAP case template provides a consistent input format for:

- Drafting first-pass Statistical Analysis Plan (SAP) outlines
- Routing endpoint-specific method logic
- Standardizing assumptions, covariates, and follow-up items
- Supporting reproducible agent-based statistical planning

---

## File Format

SAP cases are stored as YAML files.

Example location in project workflows:

```text
ct06_sap_outline_agent/data/processed/
```

Reference template location:

methods-library/agent-logic/biostatistics/sap/sap_case_template.yaml

## Core Structure

Each SAP case contains four types of information:

- Study metadata
- SAP planning fields
- Endpoint definition
- Endpoint-specific method fields

## Field Reference

Study Metadata

**study_id**

Unique study identifier.

Example:
study_id: "STUDY-001"

**analysis_id**

Unique identifier for the SAP case.

Example:
analysis_id: "sap_case_001"

**study_design**

High-level study design description.

Example values:

- randomized_controlled_trial
- observational_study

## Endpoint Definition

**endpoint_type**

Determines which method logic is used.

Allowed values:

- time_to_event
- binary
- continuous

**endpoint_name**

Human-readable endpoint name.

Examples:

- overall_survival
- objective_response_rate
- change_from_baseline_in_hba1c

## SAP Planning Fields

**objective**

Primary analysis objective.

Example:

objective: "Compare treatment groups for the specified endpoint."

**estimand_strategy**

High-level estimand strategy.

Example values:

- treatment_policy
- hypothetical
- while_on_treatment

**analysis_population**

Primary analysis population.

Example values:

- itt
- mitt
- per_protocol
- safety

**covariates**

List of baseline covariates for adjusted analysis planning.

Example:

covariates:
  - "age"
  - "sex"
  - "baseline_value"

**stratification_factors**

Factors used in randomization or planned stratified analysis.

**intercurrent_events**

List of relevant intercurrent events to align with estimand strategy.

**missing_data_risk**

Expected missing data concern level.

Allowed values:

- low
- moderate
- high

**notes**

Optional free-text planning note.


## Endpoint-Specific Fields

### Time-to-Event Fields

Used when:

endpoint_type: "time_to_event"

**ph_assumption_status**

Status of proportional hazards assumption.

Allowed values:

- holds
- violated
- unknown

**censoring_concern**

Assessment of censoring mechanism.

Example values:

- non_informative
- informative
- unknown

### Binary Endpoint Fields

Used when:

endpoint_type: "binary"

**event_rate**

Expected or observed event proportion.

Example:

event_rate: 0.18

**expected_cell_count**

Assessment of expected cell adequacy.

Allowed values:

- adequate
- low

### Continuous Endpoint Fields

Used when:

endpoint_type: "continuous"

**n_groups**

Number of groups being compared.

Example:

n_groups: 2

**sample_size**

Total sample size.

**normality**

Distributional assessment.

Allowed values:

- normal
- non_normal
- unknown

**variance_homogeneity**

Variance assumption status.

Allowed values:

- equal
- unequal
- unknown

## Example Template

study_id: "STUDY-XXX"
analysis_id: "sap_case_XXX"

study_design: "randomized_controlled_trial"

endpoint_type: "time_to_event"
endpoint_name: "overall_survival"

objective: "Compare treatment groups for the specified endpoint."

estimand_strategy: "treatment_policy"
analysis_population: "itt"

covariates:
  - "age"
  - "sex"

stratification_factors:
  - "region"

intercurrent_events:
  - "treatment_discontinuation"

missing_data_risk: "moderate"

ph_assumption_status: "unknown"
censoring_concern: "non_informative"

event_rate: 0.20
expected_cell_count: "adequate"

n_groups: 2
sample_size: 200
normality: "normal"
variance_homogeneity: "equal"

notes: "Optional description of the analysis scenario."

## Required vs Optional Fields

Typically Required:
study_id
analysis_id
study_design
endpoint_type
endpoint_name
objective
analysis_population

Strongly Recommended:
estimand_strategy
covariates
stratification_factors
intercurrent_events
missing_data_risk

Conditionally Required:
If endpoint_type: time_to_event
- ph_assumption_status
- censoring_concern

If endpoint_type: binary
- event_rate
- expected_cell_count

If endpoint_type: continuous
- n_groups
- sample_size
- normality
- variance_homogeneity

## Relationship to CT06

The CT06 SAP Outline Agent uses this template to:

- identify endpoint type
- route to the correct method logic
- draft SAP sections
- generate follow-up items
- propagate warnings and notes into reports

## Relationship to Shared Logic

This template supports integration with:

framework/shared_logic/tte_method_logic.py
framework/shared_logic/binary_method_logic.py
framework/shared_logic/continuous_method_logic.py

It acts as the input contract between SAP planning and endpoint-specific 
method logic.

## Key Principle

> A consistent SAP input schema improves reproducibility, 
supports agent-based planning, and reduces ambiguity in 
statistical decision workflows.