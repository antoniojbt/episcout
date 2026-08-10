# Brief

Spec ID: `031-canonical-eda-delivery`  
Status: Draft  
Owner: repository-owner  
Tracking issue: #245

## Problem

`epi_eda_db_run()` ends at a technically complete aggregate bundle. Users have no portable human entry point unless they return to row-level data and the in-memory renderer. The flat bundle also mixes presentation, QA and provenance files, making navigation and artifact ownership harder than necessary.

## Goal

Add an opt-in canonical database-EDA delivery layout and an additive renderer that consumes a completed validated aggregate bundle. HTML becomes the normal human entry point; owned CSV, SVG, checksums and provenance remain the reproducible evidence.

## Non-goals

- Changing `epi_eda_render_report()` or the default flat database bundle.
- Querying PostgreSQL, accepting connections/SQL/credentials, or extracting observations during rendering.
- Publishing row-level plot data or retained coordinate/theme observations.
- Adding approval, disclosure, sharing, privacy-classification or output-suppression policy.
- PDF, Word, dashboards, web maps, external assets or project-specific presentation.
- Implementing issue #248's denominator and percentage extensions.

## Candidate Files

- `R/eda_db_run.R` and new `R/eda_db_report.R`
- new `inst/report-template/eda-db.qmd`
- `tests/testthat/test-eda-db-report.R` and live database bundle tests
- README, NEWS and the installed database walkthrough

## Risks

- Mutating a valid bundle non-atomically could leave its manifest inconsistent.
- Treating feature-level maps as aggregate plot data could retain unrequested observations.
- Supporting both layouts could accidentally change existing manifest paths or overwrite fingerprints.
- HTML could link outside the owned root or depend on the original absolute location.

## Successor or Terminal Outcome

- Successor issue: #248 after #245 is merged and closed out.
