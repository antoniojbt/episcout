# Test Design

Spec ID: `006-synthetic-integer-generation`
Status: Implemented

## Test File

- `tests/testthat/test-eda_synthetic-fixtures.R`

## Behaviour Tests

- [x] A positive singleton range always returns its sole value.
- [x] A normal integer range returns only inclusive candidates.
- [x] `n = 0` returns a zero-row data frame with the specified integer column.
- [x] The same seed continues to produce identical output.

## Failure Tests

- [x] Fractional bounds containing no integer fail with an actionable error.

## Guardrail Tests

- [x] Required fixture columns have observed values before level/range checks.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-eda_synthetic-fixtures.R', reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```
