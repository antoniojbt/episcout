# Brief

Spec ID: `010-canonical-eda-summary-contract`  
Status: Draft  
Owner: Antonio Berlanga-Taylor  

## Problem

The current EDA summary interface presents two outputs as versions `v1` and `v2`, although the distinction combines historical output shape, statistical corrections and presentation coverage. The default `v1` path is not an exact reproduction of the released behaviour, while the `v2` path is the more complete typed result. This naming makes it unclear which result is authoritative and allows workflow, CSV and report output to depend on a compatibility distinction that has no continuing user requirement.

## Goal

Define and implement one authoritative EDA summary contract covering numeric, integer, categorical, binary, text, date and datetime variables. Make `epi_eda_profile_summaries()`, `epi_eda_run()`, report rendering and typed `epi_stats_summary()` output derive from the same calculation path, with explicit variable coverage, missingness, denominators, exclusions and failure reasons.

## Observable Outcome

- `epi_eda_profile_summaries(data, spec)` returns one documented six-component result: `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped`.
- `epi_eda_run()` writes the same six summary components and `epi_eda_render_report()` presents them without a version argument or alternate calculation path.
- Active package code, generated documentation, README, NEWS and the EDA vignette no longer describe EDA summaries as v1 or v2.
- No compatibility adapter is retained because external compatibility is not required for this change.

## Non-goals

- Preserving the two-table legacy summary output or accepting `summary_version` after implementation.
- Changing the extended database dictionary schema, database inventory behaviour or catalogue-profiling safety rules.
- Redesigning correlation, contingency, outcome, multivariable or plotting statistics.
- Adding a new package dependency or a new summary class solely for presentation.
- Creating a tag, preparing a release number or publishing a release.

## Candidate Files

- `R/eda_summaries.R`
- `R/summary_cores.R`
- `R/epi_stats_summary.R`
- `R/run_eda.R`
- `R/eda_report.R`
- `tests/testthat/test-summary-cores.R`
- `tests/testthat/test-epi_stats_numeric.R`
- `tests/testthat/test-eda-summaries-v2.R`
- `tests/testthat/test-eda-summaries.R`
- `tests/testthat/test-eda_summaries-fixtures.R`
- `tests/testthat/test-run_eda-fixtures.R`
- `tests/testthat/test-eda_report.R`
- `tests/testthat/test-db-dictionary.R`
- `README.md`
- `NEWS.md`
- `vignettes/specification-first-eda.Rmd`
- `inst/report-template/eda.qmd`
- Generated `man/` files and `NAMESPACE` if roxygen output changes them.

## Risks

- Removing `summary_version` and changing the default return shape breaks callers of the released two-table output; this break is explicitly authorised because external compatibility is not required.
- Existing tests may encode prior output rather than independently establish correct analytical behaviour.
- Shared cores also serve public statistics wrappers, so the authorised all-missing-sum correction must be documented and tested in `epi_stats_numeric()` while unrelated `epi_stats_*` schemas and calculations remain unchanged.
- Report and CSV output can appear successful while silently omitting variables unless coverage and skipped reasons are reconciled against every specification row.
