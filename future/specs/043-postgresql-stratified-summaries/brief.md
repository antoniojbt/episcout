# Brief

Spec ID: `043`
Status: Review
Owner: Codex
Tracking issue: issue-313

## Problem

`epi_eda_profile_stratified()` accepts only data frames, so database consumers must collect row-bearing analysis columns before producing the existing `epi_eda_stratified` and Table 1 contracts. The maintained PostgreSQL source already supports schema, missingness, typed summaries, QC and plots through aggregate queries.

## Goal

Allow an unmodified `epi_eda_postgres_source` to produce the released stratified component contract for exactly one reviewed categorical or binary stratifier. Preserve group ordering, explicit missing strata, declared zero-count levels, unexpected observed levels, canonical denominators and aggregate-only query behaviour. Allow `epi_eda_db_run()` to publish stratified components and Table 1 only when explicitly requested.

## Non-goals

- Multiple stratifiers, weights, p-values, hypothesis tests or models.
- Project-specific names, policies or disclosure decisions.
- Collecting identifiers, text examples or numeric analysis vectors.
- PostgreSQL-side cleaning or semantic inference.

## Candidate Files

- `R/eda_stratified.R`
- `R/eda_postgres_queries.R`
- `R/eda_db_run.R`
- `tests/testthat/test-eda-stratified-postgres.R`
- `README.md`
- `vignettes/specification-first-eda.Rmd`

## Risks

- Group filters could alter missing-sentinel semantics or bind unsafe values unless they are constructed from validated contracts and driver-quoted literals.
- Reusing canonical PostgreSQL summaries could accidentally invoke the bounded Shapiro value-vector query; the stratified path must disable that query and record the limitation.
- Component order or denominator drift would break `epi_eda_table1()` and categorical display consumers even if counts appear plausible.

## Successor or Terminal Outcome

- Successor issue: none.
- Terminal reason: issue-313 is complete when its bounded single-stratifier contract passes; broader analytical features require independent requirements.
