# ct01 Design Results

## Overview

This folder contains outputs from the ct01 trial design evaluation.

The design represents a Phase II two-arm randomized clinical trial with a
binary endpoint.

---

## Assumptions

* Control event rate: 0.35
* Treatment event rate: 0.55
* Absolute treatment effect: 0.20
* Significance level (alpha): 0.05
* Target power: 0.80
* Sample size: 96 subjects per arm
* Number of simulations: 5000

---

## Key Results

### Analytical Power

* Target power: 0.80
* Estimated sample size required: 96 subjects per arm

### Simulation Results

* Mean control proportion: ~0.35
* Mean treatment proportion: ~0.55
* Empirical power at 20% effect: ~0.80–0.81

The simulated event rates are consistent with the assumed parameters,
indicating correct data generation.

---

## Interpretation

The ct01 design (96 subjects per arm) is appropriately powered relative
to the target of 80% power.

Simulation results confirm that, under the assumed treatment effect,
the design achieves approximately 80% power.

Analytical and simulation-based results are aligned, supporting the
adequacy of the proposed sample size.

---

## Operating Characteristics

Operating characteristics were evaluated across multiple scenarios,
including null and non-null treatment effects.

* Type I error is controlled near the nominal 0.05 level
* Power increases appropriately with increasing treatment effect
* Confidence interval coverage is consistent across scenarios
* Power is stable across a range of plausible control event rates

---

## Notes

* Results are dependent on the specified assumptions
* This example demonstrates a design calibrated to meet a target power level
* The framework can be extended to alternative endpoints and trial designs
