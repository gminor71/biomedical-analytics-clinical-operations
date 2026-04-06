# PM01 Output Recommender

This module provides lightweight recommendation logic for project management
artifacts and governance outputs.

Given a structured project profile, it recommends:
- required outputs
- recommended outputs
- governance cadence
- tracking expectations
- escalation triggers

## Purpose

The goal is to support consistent project setup and execution by identifying
which project management artifacts are most appropriate for a given project
context.

## Inputs

The recommender evaluates:
- project type
- functional area
- complexity
- regulatory environment
- team structure
- vendor involvement
- timeline pressure
- project phase
- primary outputs

## Outputs

The recommender returns:
- required_outputs
- recommended_outputs
- governance_recommendations
- tracking_recommendations
- escalation_triggers

## Template References

Where applicable, the recommender returns references to standardized
project management templates located in the top-level `project-management/templates/`
directory.

This creates a direct link between recommendation logic and reusable
documentation artifacts.

## Position in the Repository

This module serves as a decision-support layer for the broader
project-management framework and complements the static templates
contained in the top-level templates/ directory.