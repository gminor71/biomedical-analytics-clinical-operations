# Example Design Results

## Overview

This folder contains outputs from the ct00_design example trial design
evaluation.

The design assumes a two-arm randomized trial with a binary endpoint.

---

## Assumptions

* Control event rate: 0.30
* Treatment event rate: 0.45
* Absolute treatment effect: 0.15
* Significance level (alpha): 0.05
* Sample size: 120 subjects per arm
* Number of simulations: 5000

---

## Key Results

### Analytical Power

* Target power: 0.80
* Estimated sample size required: ~163 subjects per arm

### Simulation Results

* Mean control proportion: 0.3009
* Mean treatment proportion: 0.4492
* Empirical power: 0.6696

The simulated event rates are consistent with the assumed parameters,
indicating correct data generation.

---

## Interpretation

The example design (120 subjects per arm) is underpowered relative to the
target of 80% power.

Simulation results demonstrate that, under the assumed treatment effect,
the design achieves approximately 67% power.

Analytical power calculations suggest increasing the sample size to
approximately 163 subjects per arm to meet the 80% power target.

---

## Operating Characteristics

Operating characteristics were evaluated across multiple scenarios,
including null and non-null treatment effects.

* Type I error is controlled near the nominal 0.05 level
* Power increases with increasing treatment effect
* Confidence interval coverage is consistent across scenarios

---

## Notes

* Results are dependent on the specified assumptions
* This example demonstrates how simulation can identify underpowered designs
* The framework can be extended to alternative endpoints and trial designs
