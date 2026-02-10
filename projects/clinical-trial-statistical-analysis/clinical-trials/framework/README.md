# Analysis Framework

This directory contains shared utilities supporting consistent statistical analysis and reporting workflows across clinical trial projects within this workspace.

The framework is intended to support consistent, reproducible workflows while
keeping project-specific statistical analyses self-contained.

## Purpose

The framework provides:

- Standardized path handling and project directory management
- Reusable utilities supporting standardized data preparation and analysis workflows 
- Common utilities used across Tables, Figures, and Listings (TFL) generation
- Shared functions that reduce duplication across endpoint analyses

## Design Principles

- Statistical models and analysis logic remain within individual projects
- Framework utilities do not contain project-specific assumptions
- Functions are written to support reproducibility and transparency
- Project outputs are generated from source code rather than manual editing

## Usage

Framework functions are sourced by individual projects as needed.  
Each clinical trial project remains independently executable.

## Notes

This framework is intended as a lightweight support layer rather than a
full analysis package. The goal is to standardize workflow structure while
maintaining clarity of statistical implementation within each project.
