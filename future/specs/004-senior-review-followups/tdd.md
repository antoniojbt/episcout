# Test Design

Spec ID: `004-senior-review-followups`  
Status: Implemented  

## Baseline Commands

Run before package-code implementation where practical:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
```

## Test Files

- `tests/testthat/test-eda_missing-fixtures.R`
- `tests/testthat/test-eda_summaries-fixtures.R`
- `tests/testthat/test-eda_schema-fixtures.R`
- `tests/testthat/test-plot-parallel.R`
- `tests/testthat/test-eda_report.R`

## Required Tests

- [x] `epi_eda_profile_missing()` counts standard `NA` plus one sentinel missing
      code.
- [x] `epi_eda_profile_missing()` counts multiple semicolon-separated sentinel
      codes.
- [x] `epi_eda_profile_missing()` treats absent or empty `missing_codes` as
      standard `NA` only.
- [x] `epi_eda_profile_missing()` returns `NA_real_` proportions for zero-row
      data.
- [x] `epi_eda_profile_summaries()` returns stable `NA_real_` numeric statistics
      for all-missing variables.
- [x] `epi_eda_profile_summaries()` excludes sentinel codes from numeric
      summaries.
- [x] `epi_eda_profile_summaries()` keeps declared categorical levels with zero
      counts.
- [x] `epi_eda_profile_summaries()` keeps `p` as total-row denominator and adds
      `p_observed` as observed-row denominator.
- [x] EDA schema tests document current descriptive behavior for integer/numeric
      and logical/binary cases.
- [x] Parallel plotting missing-dependency checks fail with clear suggested
      package messages.

## Anti-circularity

Expected summary outputs must not be calculated by copying implementation logic.
Use small hand-computed data frames or committed expected fixture CSVs with
transparent values.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "devtools::load_all(); testthat::test_file('tests/testthat/test-eda_missing-fixtures.R')"
scripts/rscript_env_caller.R -e "devtools::load_all(); testthat::test_file('tests/testthat/test-eda_summaries-fixtures.R')"
scripts/rscript_env_caller.R -e "devtools::load_all(); testthat::test_file('tests/testthat/test-eda_schema-fixtures.R')"
scripts/rscript_env_caller.R -e "devtools::load_all(); testthat::test_file('tests/testthat/test-plot-parallel.R')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```
