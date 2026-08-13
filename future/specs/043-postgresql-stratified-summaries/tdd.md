# Test Design

Spec ID: `043`
Status: Active

## Test Files

- `tests/testthat/test-eda-stratified-postgres.R`
- `tests/testthat/test-eda-postgres-parity.R`
- `tests/testthat/test-eda-table1.R`
- `tests/testthat/test-eda-categorical-display.R`

## Baseline Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(table1|postgres-parity|categorical-display)', reporter = 'summary')"
```

Baseline on commit `51a3c64012efa11bb33633d1dd39f8c6981e2009`: focused offline tests passed and nine opt-in PostgreSQL cases skipped as expected.

## Behaviour Tests

- [x] A hand-derived fixture proves exact groups, counts, missingness, numeric summaries, text diagnostics, temporal summaries and categorical denominators.
- [x] PostgreSQL output has the released component schemas and passes unchanged Table 1 and categorical-display consumers.
- [x] Supported aggregate fields match the data-frame path; the deliberate Shapiro limitation is explicit and tested.
- [x] Optional bundle publication owns all stratified components and Table 1 without changing defaults.

## Edge-case Tests

- [x] Zero rows, all-missing strata, missing-stratum exclusion, declared empty groups/levels and unexpected groups/levels preserve deterministic accounting.
- [x] Non-finite numeric and UTC temporal values preserve canonical aggregate semantics.
- [x] Unsupported and missing variables remain audited in skipped output.

## Failure Tests

- [x] Modified sources, unsafe identifiers, caller transactions, incompatible stratifiers and source drift fail before analytical queries; existing missing-contract tests cover invalid sentinels.
- [x] SQL-boundary instrumentation proves that neither direct stratification nor the opt-in bundle fetches an unaggregated analysis-value vector.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(stratified-postgres|table1|categorical-display|postgres-parity)', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-stratified-postgres', reporter = 'summary')"
scripts/check-local.sh
scripts/check-cran.sh
```
