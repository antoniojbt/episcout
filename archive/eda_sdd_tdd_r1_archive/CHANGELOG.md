# Specification-first EDA Planning Changelog

## 2026-06-15

- Archived the completed SDD/TDD planning docs under
  `archive/eda_sdd_tdd_r1_archive/`.
- Moved remaining next-phase work into root-level `future/specs/`.
- Added root-level `future/` SDD/TDD workspace for future development.

## Branch History Summary

- Added the specification-first EDA SDD, ADRs, roadmap, TDD instructions and
  review checklist.
- Added external fixture files and fixture-backed tests for the initial EDA
  contracts.
- Implemented specification loading, validation, schema checks and missingness
  profiling.
- Added synthetic-data tests and implemented synthetic data generation.
- Added summary and plot tests and implemented summary/plot profiling.
- Added `epi_eda_run()` workflow tests and implementation.
- Added report-rendering tests and `epi_eda_render_report()`.
- Added project-template tests, `inst/project-template/`, and
  `epi_eda_create_project()`.
- Completed CRAN-check and Rd documentation cleanup on the branch.
