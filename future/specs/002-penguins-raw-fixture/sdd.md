# Software Design

Spec ID: `002-penguins-raw-fixture`  
Status: Ready for activation  

## Scope

Add a second external fixture for standard EDA contracts using
`palmerpenguins::penguins_raw`.

The fixture should complement `blood_storage`:

- `blood_storage`: biomedical fixture for clinical schema, missingness and
  workflow tests.
- `penguins_raw`: standard public EDA fixture for raw column names, mixed
  variable types, missingness, summaries, plot-dispatch contracts and report
  examples.

## Required Fixture Files

```text
tests/testthat/fixtures/penguins_raw/SOURCE.md
tests/testthat/fixtures/penguins_raw/penguins_raw.csv
tests/testthat/fixtures/penguins_raw/penguins_raw_spec.csv
tests/testthat/fixtures/penguins_raw/expected_missing.csv
tests/testthat/fixtures/penguins_raw/expected_summary_numeric.csv
tests/testthat/fixtures/penguins_raw/expected_summary_categorical.csv
tests/testthat/fixtures/penguins_raw/expected_plot_inventory.csv
```

## Regeneration Script

Create:

```text
data-raw/test-fixtures/make_penguins_raw_fixture.R
```

The script may require `palmerpenguins`, but routine tests must use committed
CSV files and must not require `palmerpenguins` or internet access.

## Anti-circularity

Expected outputs must be computed with base R, simple transparent code or
manual review. The regeneration script must not call `episcout`, `episcout::`
or any `epi_eda_*` function under test.
