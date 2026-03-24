# ct00_design – Results

## Overview

This directory contains outputs generated from trial design evaluation workflows,
including simulation results, analytical power calculations, and operating
characteristics.

The `ct00_design` module serves as a sandbox and template for design-phase
statistical evaluation prior to formal analysis.

---

## Structure

### example/

Contains a fully worked example of a two-arm randomized trial with
a binary endpoint.

This example demonstrates:

* Analytical power calculation
* Simulation-based validation
* Operating characteristic assessment
* Interpretation of underpowered vs adequately powered designs
* The example provided here intentionally demonstrates an 
underpowered design scenario.

Outputs include:

* Simulation results and summary statistics
* Power curve (data and figure)
* Operating characteristics across multiple scenarios
* Narrative interpretation of results

---

## Usage

This folder is intended to:

1. Demonstrate how trial design evaluation is performed
2. Provide a reusable template for project-specific design workflows
3. Serve as a reference for interpreting power, bias, and coverage

---

## Relationship to Project-Level Design

In applied projects (e.g., `ct01`, `ct02`, etc.):

* Design scripts are copied and adapted into project-specific `design/` folders
* Outputs are written to `design/results/` within each project
* Design results are integrated into the reporting layer (e.g., results narrative)

The `ct00_design` module should be treated as a **template and reference implementation**, 
not as a production analysis.

---

## Notes

* Outputs in this directory are example artifacts and may not reflect finalized
  study designs
* Assumptions (event rates, effect sizes, sample size) are configurable via
  parameter files
* This framework can be extended to additional endpoint types (continuous,
  time-to-event, count)
