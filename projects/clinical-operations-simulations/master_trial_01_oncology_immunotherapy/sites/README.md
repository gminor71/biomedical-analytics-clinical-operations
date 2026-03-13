# sites/README.md

# Sites

This folder contains site-specific environments used throughout the master trial simulation.

Each site represents a distinct operational profile designed to create realistic variation across the trial.

---

## Site Structure

Each site folder typically includes:

* `site_profile.md`
* local context or notes
* site-level reference materials

---

## Simulation Roles

Current site design:

* **Site 001 — Model Site**
  Stable performance, low operational risk.

* **Site 002 — Typical Site**
  Normal variability and minor operational friction.

* **Site 003 — High Enrolling Site**
  Primary CRA focus; increased workload and monitoring complexity.

* **Site 004 — Safety-Focused Site**
  Elevated safety oversight and escalation potential.

---

## Purpose

Site folders represent:

* local site behavior
* operational differences between sites
* context for monitoring decisions

Not all variation represents problems — normal differences are expected.

---

## Working Guidance

* Site folders describe context, not timeline events.
* Changes in site performance over time should be reflected through scenario phases in `timeline/`.
