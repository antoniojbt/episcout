# Brief

Spec ID: `015-data-intake-to-report-workflow`
Status: Completed

## Problem

The package now has conservative public components for specification scaffolding, reviewed preparation, canonical descriptive summaries and one-variable stratification, but a new analyst must discover their ordering, manually enforce their review gates and assemble several machine-readable outputs. The existing `epi_eda_run()` and `epi_eda_render_report()` predate those contracts: they assume a supplied specification, do not provide audited preparation or a review-required state, and write directly into an existing directory without a workflow manifest.

## Goal

Add one stage-gated public workflow that receives an in-memory dataset, creates an editable scaffold when necessary, audits or applies a reviewed specification, calculates canonical and optional stratified descriptive summaries, and writes a traceable report bundle. It must preserve inspect-before-transform behaviour and return an inspectable run object at ordinary review gates rather than guessing semantics or leaving a misleading complete report.

## User Outcome

An analyst can call the workflow first to obtain a privacy-conscious scaffold and review guide, call it again to audit the reviewed specification, resolve any blocker, then deliberately apply preparation and create deterministic descriptive outputs with an optional Table 1 and HTML view. Status, provenance, messages and the manifest make clear what ran, what did not run and what requires human action.

## Scope

- One public orchestration entry point, `epi_eda_intake_run()`.
- In-memory data frames only; `epi_read()` remains the separate convenience importer.
- Specification scaffolding through `epi_eda_spec_scaffold()` and validation through `epi_eda_spec()`.
- Audit/all-or-nothing preparation through `epi_eda_prepare()` with its conservative default level and extra-variable policies.
- Canonical missingness and six-component descriptive summaries through current EDA APIs.
- Optional single-variable stratification and Table 1 through spec 014 APIs.
- A stable run object, explicit input/spec/report state, structured messages, a fixed created/not-created artifact registry, portable relative paths, provenance and checksums.
- A base-R, HTML-escaped human-readable view built only from saved CSV artifacts.
- A user-facing two-call walkthrough and synthetic end-to-end tests.

## Non-goals

Arbitrary import formats, database/API/cloud ingestion, row-level cleaned-data export, automatic semantic approval, automatic PII detection, pseudonymisation, bridge-table handling, disclosure control, hypothesis testing, modelling, graphical wizards, workflow schedulers, remote upload, telemetry, background jobs, new statistical formulae, multiple stratifiers, tags and releases.

## Implementation Files

- `R/eda_intake.R`
- `R/eda_summaries.R`
- `R/eda_stratified.R`
- `tests/testthat/test-eda-intake.R`
- `README.md`
- `NEWS.md`
- `vignettes/specification-first-eda.Rmd`
- Generated roxygen outputs and `NAMESPACE`

## Risks

- Treating a generated scaffold as approved could convert or summarize data under invented semantics.
- Calling old convenience workflows could duplicate calculations, create plots outside scope or bypass the new preparation gate.
- `prepare = "none"` could become a permissive mode unless any planned transformation blocks analysis.
- A failed staging or directory swap could corrupt a prior bundle unless publication uses a sibling backup and restoration path.
- Accepting a non-empty target without reconciling manifest-created paths and checksums could replace unrelated, missing, modified or special content.
- Manifests, source paths, free-text examples, identifier values or error details could disclose subject or environment information.
- The built-in report view could recalculate, fail to escape metadata or silently format away denominators, missingness and incomplete states.
- Timestamps and absolute paths could make deterministic calculations appear irreproducible.
- Summary outputs, particularly small stratified cells, can disclose information even though no raw rows are written.

## Authority And Approval

Issue #184 and completed specs 010, 012, 013 and 014 define the orchestration contract. The owner explicitly authorised implementation of each issue on 2026-08-03. This active specification narrows ambiguous points conservatively and does not authorise new data semantics, destructive directory cleanup, Codecov work, a tag or a release.
