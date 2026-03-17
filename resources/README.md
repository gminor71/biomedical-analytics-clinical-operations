# Resources Directory

**Program:** biomedical-analytics-clinical-operations

---

## Purpose

The `resources/` directory contains **program-level standards, references, and shared guidance** that apply across multiple projects within the biomedical-analytics-clinical-operations program.

This folder is intentionally separate from project-specific templates, analysis code, and simulation artifacts.

Its purpose is to promote:

* Consistency across projects
* Shared terminology and conventions
* Standardized documentation practices
* Long-term maintainability of the program

---

## Scope

Resources placed here should be:

* Cross-disciplinary (clinical operations, biostatistics, analytics)
* Stable references rather than active working files
* Applicable across multiple project types

Examples include:

* Naming conventions
* Documentation style guides
* File organization standards
* Shared glossary of clinical research terms
* Versioning and change-control guidance

---

## What Does NOT Belong Here

To avoid duplication and confusion, the following should **not** be stored in this directory:

### Project-specific templates

Examples:

* TFL shells
* Monitoring visit report templates
* Query logs

These belong inside their respective projects (e.g., `ctXX`, `opsXX`).

---

### Runtime code or helper scripts

Examples:

* R helper functions
* Python utilities
* Analysis engines

These belong in framework or methods folders.

---

### Study-specific materials

Examples:

* Protocol-specific documents
* Scenario files
* Mock datasets
* Trial outputs

These belong inside individual project folders.

---

## Relationship to Other Program Components

### Methods Library

`methods-library/`

Contains reusable project scaffolds and templates intended to be instantiated when starting new work.

**Rule:**
Library = “start new projects from here.”

---

### Framework Layers

Example:

```
clinical-trials/framework/
```

Contains shared runtime utilities and helper scripts used across related projects.

**Rule:**
Framework = “shared execution tools.”

---

### Project Folders

Example:

```
projects/
```

Contain execution-level work, project templates, simulations, analyses, and outputs.

**Rule:**
Projects = “where the work happens.”

---

### Clinical Trials Operations Handbook

Example:

```
clinical-trials-operations-system/
```

Serves as the policy and reference layer.

Resources in this folder should support consistency with the handbook but should not duplicate operational guidance.

---

## Design Principles

1. **Single Source of Truth**
   Standards live here; execution happens elsewhere.

2. **Minimal Duplication**
   If guidance already exists in a project handbook or SOP, link to it rather than copying content.

3. **Clarity Over Complexity**
   Keep resources stable and simple so they remain useful long-term.

4. **Program-Level Perspective**
   Ask:
   *Will this help multiple projects or disciplines?*
   If yes → this folder may be appropriate.

---

## Current Contents

* versioning-and-change-control.md — guidance for managing change across standards, templates, frameworks, and project work while preserving reproducibility.

---

## Planned Standards (As Needed)

* glossary.md
* naming-conventions.md
* writing-style-guide.md
* artifact-indexing-standard.md

---

## Guiding Mental Model

```
Resources       → Standards
Methods Library → Reusable scaffolds
Frameworks      → Shared execution tools
Projects        → Applied work
```

---

## Maintainer Note

This directory is intentionally lightweight.
Add files slowly and only when they provide clear cross-program value.
