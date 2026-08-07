# Test Design

Spec ID: `019-postgresql-catalogue-missingness`
Status: Implemented

## Test Files

- `tests/testthat/test-db-dictionary.R`
- `tests/testthat/test-eda-postgres-catalogue.R`

## Baseline Evidence

The existing dictionary unit suite passed. Source inspection confirms that the
cardinality SQL ignores NULL while the grouped frequency SQL includes a NULL
row. The local environment has no PostgreSQL client; the repository's pinned
PostgreSQL 17 CI job supplies mandatory live evidence.

## Unit Tests

- [x] The result has exact class/component/column/type schemas.
- [x] Non-missing values and counts remain ordered and keyed.
- [x] Missing counts are one separate row per profiled column.
- [x] No `values$source_value` is missing.
- [x] Empty selection returns typed empty components without requiring a
  connection.
- [x] `max_levels` continues to gate the non-missing cardinality.
- [x] Generated SQL excludes NULL from grouped value rows.

## Live PostgreSQL Tests

- [ ] A zero-row table returns no value rows and `n_missing = 0` in mandatory
  PostgreSQL 17 CI.
- [ ] An all-NULL table returns no value rows and the exact aggregate missing
  count.
- [ ] A table at the non-missing limit plus NULL returns exactly the limited
  value rows and its separate missing count.
- [ ] A table one value beyond the limit refuses without returning values.
- [x] The live test uses a uniquely named, exactly scoped disposable schema and
  registers cleanup before populating it.

## Documentation Tests

- [x] Generated help states the component schemas and limit semantics.
- [x] The longitudinal guide and runnable walkthrough show separate review of
  `values` and `missing`.
- [x] NEWS records the pre-release result-shape correction.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'db-dictionary', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-postgres-catalogue', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/check-local.sh
git diff --check
```
