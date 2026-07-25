# Software Design

Spec ID: `002-penguins-raw-fixture`  
Status: Active

## Scope

Add a second external fixture for standard EDA contracts using `palmerpenguins::penguins_raw`.

The fixture should complement `blood_storage`:

- `blood_storage`: biomedical fixture for clinical schema, missingness and workflow tests.
- `penguins_raw`: standard public EDA fixture for raw column names, mixed variable types, missingness, summaries, plot-dispatch contracts and report examples.

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

Extend the existing external-fixture entry point:

```text
data-raw/test-fixtures/make_external_fixtures.R
```

The script may require `palmerpenguins`, but routine tests must use committed CSV files and must not require `palmerpenguins` or internet access.

After serialization, the generator must read the CSV fixture back with the same options used by routine tests before deriving expected outputs. Expected schema types must independently apply the documented EDA observed-type contract rather than copy package implementation code.

## Anti-circularity

Expected outputs must be computed with base R, simple transparent code or manual review. The regeneration script must not call `episcout`, `episcout::` or any `epi_eda_*` function under test.

## Executable Consumers

Add `tests/testthat/test-penguins-raw-fixtures.R`. It must load and compare the committed fixture against every `expected_*.csv` file. Plot inventory checks must inspect non-visual plot structure so they distinguish numeric from categorical/binary dispatch.
