# Test Design

Spec ID: `041-reviewed-civil-date-derivation`

Status: Active

## Independent Expectations

Use hand-authored neutral local timestamp strings and literal `Date` expectations. The values `2024-02-29 00:00:00`, `2024-12-31 00:00:00.000`, `NA` and `2025-01-01 00:00:00` derive literally to `2024-02-29`, `2024-12-31`, `NA` and `2025-01-01`. The values `12:00:00`, `00:00:01` and `00:00:00.001` are independently identifiable non-midnight cases; they are never corrected or included in expected derived output.

## Baseline Commands

```bash
scripts/check-workflow-state.sh
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-cleaning', reporter = 'summary')"
```

## Schema And Validation Tests

- [x] Assert exact public formals, classes, operation columns, canonical ordering and immutable-object revalidation.
- [x] Reject missing/extra/reordered fields, zero operations, pending state, undeclared semantics, false/missing safeguards, duplicate/malformed keys and names, malformed approval references and unresolved source mappings.
- [x] Use canary names, values, paths and relation identities and confirm validation errors and display output disclose none of them.

## In-Memory Behaviour Tests

- [x] Derive literal `Date` values from valid midnight character timestamps across leap-day, year-end and year-start boundaries while preserving source values, source object, rows, row order and row names.
- [x] Preserve missing values exactly; exercise all-missing and zero-row sources.
- [x] Accept zero-valued fractional seconds and block non-zero seconds or fractional seconds with only the aggregate affected-value count.
- [x] Reject malformed calendar/time text, timezone or offset syntax, `POSIXct`/`POSIXlt` and destination-name collisions before creating any partial column.
- [x] Publish complete RDS/CSV results and inject a derivation failure to confirm no destination or staging file exists.

## PostgreSQL Behaviour Tests

- [x] Inspect constructed SQL under a mocked connection to confirm quoted source/derived identifiers, `::time` midnight validation and `::date` derivation without timezone syntax or row values.
- [x] In a disposable PostgreSQL 18 database, compare supported midnight/missing/calendar-boundary results and aggregate audit counts with the in-memory fixture.
- [x] Confirm `timestamp with time zone`, non-midnight values, fractional seconds, non-finite timestamps and destination-name collisions block with no destination table, no source mutation and an idle connection.
- [x] Exercise zero rows and inject post-creation catalogue and dimension reconciliation failures to confirm complete rollback.

## Broader Checks

Style the new R source, regenerate roxygen output, run package lint, render and inspect the changed vignette, apply the software-verification, truth-and-semantics, analysis-and-statistics and copy-edit checklists, run workflow-state validation and run the complete local package check.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-civil-dates', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-civil-dates', reporter = 'summary')"
scripts/check-workflow-state.sh
scripts/check-local.sh
```
