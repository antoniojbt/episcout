# Brief

Spec ID: `049`
Status: Active
Owner: MacBotticus
Tracking issue: [#347](https://github.com/antoniojbt/episcout/issues/347)

## Problem

Episcout has canonical single-period PostgreSQL descriptive summaries but no aggregate-only, longitudinal comparison for a reviewed set of variables across ordered period relations.

## Goal

Add `epi_eda_longitudinal_drift(sources, spec, variables = NULL, max_levels = 50L)`. It reports descriptive schema continuity, missingness, numeric, categorical and temporal change while retaining raw values and identifiers in PostgreSQL. It is evidence for a downstream longitudinal QC report, not anomaly detection, cleaning or scientific interpretation.

## Non-goals

- Entity retention, entry, exit or gap QC (#346).
- State-transition matrices (#348).
- Thresholds, anomaly labels, inference, tests, cleaning or dictionary mutation.
- Oferta, SIAP, CURP or project terminology.

## Frozen Authority

The GitHub issue’s section **“Frozen implementation decisions after #346 closeout (2026-08-17)”** is the controlling contract. Its twelve decisions are reproduced verbatim in `sdd.md`, with executable acceptance in `tdd.md` and `acceptance.md`.

## Candidate Files

- `R/eda_longitudinal_drift.R`
- `R/eda_longitudinal_qc.R`
- `R/eda_postgres_queries.R`
- `tests/testthat/test-eda-longitudinal-drift.R`
- `tests/testthat/test-eda-longitudinal-postgres-drift.R`

## Successor

- Successor issue: #348, state transitions.
