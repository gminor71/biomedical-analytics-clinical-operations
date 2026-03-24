# ct00_design

Clinical trial design framework for simulation-based planning and operating
characteristic assessment.

---

## Purpose

This module provides a reusable framework for evaluating clinical trial designs
before formal analysis.

It supports:

* analytical power and sample size estimation
* simulation of trial outcomes under assumed parameters
* evaluation of operating characteristics (type I error, power, bias, CI coverage)

---

## Structure

* `template/`
  Reusable design scripts intended to be copied and adapted for project-specific
  trial designs (e.g., ct01–ct04).

* `example/`
  Executable example demonstrating a two-arm randomized trial with a binary
  endpoint.

* `results/example/`
  Outputs generated from the example design scenario.

---

## Design Workflow

The standard design workflow is:

1. Define trial assumptions

   * control event rate
   * treatment effect
   * sample size
   * alpha and target power

2. Run `02_power_analysis.R`

   * estimate required sample size
   * evaluate analytical power

3. Run `01_simulate_trial.R`

   * simulate repeated trials under the assumed design
   * assess variability and empirical performance

4. Run `03_operating_characteristics.R`

   * evaluate robustness across multiple scenarios
   * assess type I error, power, bias, and CI coverage

---

## How to Use in Projects

For a new project (e.g., `ct01_binary_endpoint_trial_analysis`):

1. Copy scripts from `template/` into the project’s `design/` folder
2. Modify `00_design_parameters.R` for project-specific assumptions
3. Run design scripts within the project context
4. Store outputs under the project’s `results/design/` directory

---

## Notes

* Current implementation focuses on a binary endpoint example

* Framework can be extended to:

  * continuous endpoints
  * count data
  * time-to-event analyses

* Design results are assumption-dependent and should be interpreted within the
  context of the specified parameters
