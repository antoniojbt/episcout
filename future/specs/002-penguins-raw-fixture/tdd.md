# Test Design

Spec ID: `002-penguins-raw-fixture`  
Status: Ready for activation  

## Test Files

- `tests/testthat/test-fixture-generation-guardrails.R` if lightweight.
- Existing fixture-backed EDA tests may consume the committed files in later
  implementation specs.

## Fixture Expectations

- [ ] `penguins_raw.csv` has 344 rows and 17 columns.
- [ ] `SOURCE.md` records source package, package version, row count, column
      count, documentation URLs and regeneration command.
- [ ] `penguins_raw_spec.csv` is manually reviewed and uses source variable
      names exactly as provided.
- [ ] `expected_missing.csv` is independently computed.
- [ ] `expected_summary_numeric.csv` is independently computed.
- [ ] `expected_summary_categorical.csv` is independently computed.
- [ ] `expected_plot_inventory.csv` records plot-dispatch expectations without
      visual snapshots.

## Guardrail Test

If added, the guardrail test should scan fixture-generation scripts and fail on
forbidden calls:

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
scripts/rscript_env_caller.R -e "devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::check(manual = FALSE)"
```
