# Brief

Spec ID: `019-postgresql-catalogue-missingness`
Status: Implemented
Owner: repository-owner

## Problem

`epi_db_catalogue_profile()` gates on distinct non-missing values but groups
PostgreSQL `NULL` into an additional `source_value = NA` row. A result allowed at
`max_levels` can therefore contain one extra row, and the missing source value
cannot be copied into the normalised catalogue contract.

## Goal

Return directly reviewable non-missing value counts separately from one
aggregate PostgreSQL NULL count per profiled column. Keep `max_levels` as the
bound on distinct non-missing catalogue values and make zero-row, all-NULL,
exactly-at-limit and limit-plus-one behavior explicit.

## Non-goals

- Automatically classify PostgreSQL NULL or any observed value as a reviewed
  missing code.
- Change catalogue validation, database contents or privacy profiling gates.
- Add generic DBI backend support or query row-level identifiers.
- Implement deferred database report rendering.

## Candidate Files

- `R/eda_dictionary.R`
- `tests/testthat/test-db-dictionary.R`
- `tests/testthat/test-eda-postgres-catalogue.R`
- `man/epi_db_catalogue_profile.Rd`
- `vignettes/longitudinal-pseudonymisation.Rmd`
- `inst/examples/db-to-report/walkthrough.R`
- `NEWS.md`
- planning and closeout records under `future/`

## Risks

- The return type changes from a flat data frame to a named two-component list.
- Repeating or dropping aggregate missing counts could make all-NULL/empty
  sources indistinguishable.
- Tests that reuse the production query could miss PostgreSQL NULL semantics.
