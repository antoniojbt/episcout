# Test Design

Spec ID: `002-penguins-raw-fixture`  
Status: Implemented

## Test Files

- `tests/testthat/test-penguins-raw-fixtures.R` must consume the source data, specification and every committed expected-output CSV.
- `tests/testthat/test-fixture-generation-guardrails.R` if lightweight.

## Fixture Expectations

- [x] `penguins_raw.csv` has 344 rows and 17 columns.
- [x] `SOURCE.md` records source package, package version, row count, column
      count, documentation URLs and regeneration command.
- [x] `penguins_raw_spec.csv` is manually reviewed and uses source variable
      names exactly as provided.
- [x] `expected_schema.csv` is independently computed from serialized data.
- [x] `expected_missing.csv` is independently computed.
- [x] `expected_summary_numeric.csv` is independently computed.
- [x] `expected_summary_categorical.csv` is independently computed.
- [x] `expected_plot_inventory.csv` records plot-dispatch expectations without
      visual snapshots.
- [x] Executable tests compare every expected-output CSV with package behavior.
- [x] Plot tests distinguish specification-based numeric, categorical and
      binary dispatch through non-visual plot structure.

## Guardrail Test

If added, the guardrail test should scan fixture-generation scripts and fail on forbidden calls:

- `library(episcout)`
- `episcout::`
- `epi_eda_spec(`
- `epi_eda_validate_spec(`
- `epi_eda_check_schema(`
- `epi_eda_profile_missing(`
- `epi_eda_profile_summaries(`
- `epi_eda_profile_plots(`
- `epi_eda_run(`
- `epi_eda_generate_synthetic_data(`

Do not run regeneration scripts from routine tests.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-penguins-raw-fixtures.R', reporter = 'summary'); testthat::test_file('tests/testthat/test-fixture-generation-guardrails.R', reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```
