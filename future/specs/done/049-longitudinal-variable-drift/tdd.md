# Test Design

Spec ID: `049`
Status: Completed

## Test Files

- `tests/testthat/test-eda-longitudinal-drift.R`
- `tests/testthat/test-eda-longitudinal-postgres-drift.R`

## Independent Truth Fixture

A neutral two-period PostgreSQL fixture has stable, missing, numeric, categorical and date/datetime fields; an additional three-period construction covers zero-row and all-missing periods. The expected per-period counts, finite numeric summaries, category counts/proportions, date ranges, and adjacent differences are hand-authored in assertions, not snapshots of the implementation.

## Behaviour Tests

- [x] Exact API validation, class and ordered component names.
- [x] Schema, missingness, numeric, categorical, temporal and skipped schemas have the frozen typed columns and order.
- [x] Stable distributions, missingness increase/decrease, numeric location/spread change, categorical proportion change, introduced/removed levels and temporal coverage have hand-derived results.
- [x] Every relevant period summary reconciles exactly to `epi_eda_profile_summaries()` called on the corresponding canonical PostgreSQL source.
- [x] Text is explicitly skipped; missing/absent/incompatible fields are nonfatal audited evidence.
- [x] Zero-row and all-missing periods retain typed unavailable metrics.
- [x] Ordering follows sources, resolved variables, declared level order and bytewise unexpected level order.

## Failure And Locality Tests

- [x] Invalid selection, changed/unapproved source, mixed connection, bad specification and catalogue/SQL failures are hard errors without an object.
- [x] `max_levels` rejects zero/non-whole values; declared levels, an observed domain of 51 values, and an adjacent union above the limit hard fail using preflights bounded to `max_levels + 1` rows.
- [x] A second connection writes during the snapshot; all drift queries retain the initial snapshot.
- [x] A forced query failure rolls back and the caller-owned connection remains open and reusable.
- [x] Normal results and errors contain no fixture identifiers or source rows.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "parse('R/eda_longitudinal_drift.R')"
scripts/rscript_env_caller.R -e "lintr::lint('R/eda_longitudinal_drift.R')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-longitudinal-drift', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-longitudinal-postgres-drift', reporter = 'summary')"
scripts/check-workflow-state.sh --offline
scripts/check-local.sh
```
