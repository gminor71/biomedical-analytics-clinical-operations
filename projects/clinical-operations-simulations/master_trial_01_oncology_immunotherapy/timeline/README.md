# timeline/README.md

# Timeline

The timeline folder is the **scenario engine** of the master trial simulation.

Each subfolder represents a milestone phase in the lifecycle of the study rather than independent case studies.

---

## Timeline Philosophy

The simulation follows an evolving operational model:

* The trial continues over time.
* Operational complexity increases gradually.
* Monitoring activities reflect realistic study progression.

---

## Timeline Phases

* `t0_startup_context` — background and setup context
* `t1_early_enrollment` — initial monitoring activities
* `t2_active_monitoring` — steady-state execution
* `t3_risk_escalation` — emerging risks and intensified oversight
* `t4_pre_database_lock` — data cleanup and closeout readiness

---

## Scenario Briefs

Each timeline phase contains a `scenario_brief.md` file that describes:

* phase purpose
* operational state
* expected CRA activities

Scenario briefs are intentionally lightweight placeholders until detailed scenarios are developed.

---

## Working Guidance

* Timeline folders represent changing operational reality.
* Trial rules remain defined in `trial_context/`.
* Generated outputs should be stored in `outputs/`.
