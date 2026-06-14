# TDD external fixtures for specification-first EDA

## Status

Draft.

## Purpose

Add a test-driven development layer for the new specification-first EDA workflow.

The existing `episcout` tests cover many helper functions, but they do not yet prove that the new workflow works end-to-end with:

- real biomedical data;
- a real data dictionary;
- expected schema results;
- expected summary results;
- expected missingness results;
- synthetic data generated from the same specification.

## Core decision

Use small, externally documented, publicly available biomedical datasets as test fixtures.

Do not use private project data.

Do not use large live downloads in routine unit tests.

Prefer pinned fixtures committed under `tests/testthat/fixtures/`, with source provenance and expected results stored beside them.

## Recommended first fixture source

Use the CRAN package `medicaldata` as the first external source.

Primary fixture:

```text
medicaldata::blood_storage
```

Secondary fixture:

```text
medicaldata::scurvy
```

Optional later fixture:

```text
medicaldata::covid_testing
```

## Why `blood_storage` first

`blood_storage` is a small but realistic retrospective cohort dataset.

Known documented properties:

```text
dataset: blood_storage
source package: medicaldata
observations: 316
variables: 20
study type: retrospective cohort
clinical area: prostate cancer recurrence after transfusion
```

It is suitable for first-pass EDA tests because it has:

- numeric variables;
- binary-coded variables;
- ordinal or exposure-style variables;
- survival or time-to-event-style variables;
- documented ranges;
- a real clinical context;
- small enough size for fast unit tests.

## Why `scurvy` second

`scurvy` is tiny and useful for edge-case testing.

Known documented properties:

```text
dataset: scurvy
observations: 12
variables: 8
study type: reconstructed RCT
```

It is suitable for:

- categorical handling;
- low-n small-data behaviour;
- plot dispatch on categorical variables;
- deterministic report tests.

## Why `covid_testing` later

`covid_testing` is larger.

Known documented properties:

```text
dataset: covid_testing
observations: 15524
variables: 17
clinical area: SARS-CoV-2 testing at CHOP in 2020
```

It is suitable for later performance-oriented tests, but should not be the first fixture because it is larger than needed for early unit tests.

## Alternative later fixture source: NHANES

NHANES is a strong external reference source because CDC provides public datasets, questionnaires, codebooks and documentation across survey waves.

It is not recommended as the first fixture because it introduces:

- survey-design complexity;
- multi-file structure;
- changing variable names across waves;
- larger documentation overhead;
- more complex provenance requirements.

NHANES may be useful later for:

- external validation of data-dictionary handling;
- multi-file workflows;
- survey-cycle metadata;
- larger real-world biomedical data tests.

## Proposed fixture layout

Add:

```text
tests/testthat/fixtures/
├─ blood_storage/
│  ├─ blood_storage.csv
│  ├─ blood_storage_spec.csv
│  ├─ expected_schema.csv
│  ├─ expected_missing.csv
│  ├─ expected_summary_numeric.csv
│  ├─ expected_summary_categorical.csv
│  └─ SOURCE.md
├─ scurvy/
│  ├─ scurvy.csv
│  ├─ scurvy_spec.csv
│  ├─ expected_schema.csv
│  ├─ expected_missing.csv
│  └─ SOURCE.md
└─ README.md
```

## Fixture generation script

Add a script that can regenerate fixtures, but do not run it during ordinary tests.

```text
data-raw/test-fixtures/make_external_fixtures.R
```

The script should:

1. Check that `medicaldata` is installed.
2. Load selected datasets.
3. Write CSV fixture files.
4. Write source metadata.
5. Write manually reviewed data dictionaries.
6. Write expected outputs using base R or simple, transparent calculations.
7. Avoid using the new `episcout` functions to generate expected results.

## Expected-results rule

Expected values must be independently computed.

Bad:

```r
expected_missing <- profile_missing(data, spec)
```

Good:

```r
expected_missing <- data.frame(
  name = names(data),
  n = nrow(data),
  n_missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
  p_missing = vapply(data, function(x) mean(is.na(x)), numeric(1))
)
```

This avoids circular tests.

## Minimum fixture-backed tests

### Specification tests

Test file:

```text
tests/testthat/test-eda_spec-fixtures.R
```

Tests:

- `blood_storage_spec.csv` loads successfully;
- required columns are present;
- variable names are unique;
- invalid type fails clearly;
- duplicate variable name fails clearly.

### Schema tests

Test file:

```text
tests/testthat/test-eda_schema-fixtures.R
```

Tests:

- `check_schema(blood_storage, blood_storage_spec)` matches `expected_schema.csv`;
- missing variable is flagged;
- unexpected variable is flagged;
- all expected variables are present in the unmodified fixture.

### Missingness tests

Test file:

```text
tests/testthat/test-eda_missing-fixtures.R
```

Tests:

- missingness output matches `expected_missing.csv`;
- all variables have correct `n`;
- missing percentages are stable with tolerance;
- all-complete variables report zero missingness.

### Synthetic-data tests

Test file:

```text
tests/testthat/test-eda_synthetic-fixtures.R
```

Tests:

- synthetic data has same variable names as the specification;
- synthetic data has requested row count;
- synthetic values respect levels where provided;
- synthetic values respect min/max where provided;
- fixed seed gives identical output;
- synthetic data can be passed into `run_eda()` once available.

### Summary tests

Test file:

```text
tests/testthat/test-eda_summaries-fixtures.R
```

Tests:

- selected numeric summaries match independently computed expected values;
- selected categorical summaries match independently computed expected values;
- summary functions return stable column names;
- all variables listed in the spec receive a summary or a documented skip reason.

### Plot tests

Test file:

```text
tests/testthat/test-eda_plots-fixtures.R
```

Tests:

- eligible variables return ggplot objects;
- plot names match variable names;
- unsupported variable types return a documented skip result;
- no plots are printed during tests.

### End-to-end tests

Test file:

```text
tests/testthat/test-run_eda-fixtures.R
```

Tests:

- `run_eda()` returns a named list;
- required components exist: metadata, schema, missing, summaries, plots;
- `run_eda()` works on real fixture data;
- `run_eda()` works on synthetic data generated from the same spec;
- output files are created in a temporary directory.

## Expected named output from `run_eda()`

The first stable contract should be:

```r
list(
  metadata = ...,
  schema = ...,
  missing = ...,
  summaries = ...,
  plots = ...
)
```

Avoid changing this contract after tests are added.

## Source metadata

Each fixture should have a `SOURCE.md` file.

Minimum contents:

```markdown
# blood_storage fixture source

Dataset: blood_storage
Source package: medicaldata
Package source: CRAN
Original dataset type: retrospective cohort
Rows: 316
Columns: 20
Purpose: specification-first EDA tests
Regeneration script: data-raw/test-fixtures/make_external_fixtures.R

This fixture is used only for package testing. It is not private project data.
```

## Dependency policy for tests

Preferred approach:

- commit small CSV fixtures under `tests/testthat/fixtures/`;
- routine tests do not require internet;
- routine tests do not require `medicaldata`;
- `medicaldata` is used only by the regeneration script.

Acceptable approach:

- put `medicaldata` in `Suggests`;
- skip regeneration tests if it is unavailable.

Do not make `medicaldata` a hard runtime dependency for `episcout`.

## TDD principle

For every new spec-first EDA function:

```text
fixture data + fixture spec + expected output first
implementation second
```

All new code should test against the external fixture where practical.

## TDD sequence

Recommended order:

```text
1. Add spec_driven_EDA_plan/docs/sdd/tdd-external-fixtures.md.
2. Add fixture generation script.
3. Commit blood_storage CSV fixture and spec.
4. Commit independently computed expected schema and missingness outputs.
5. Add failing tests for eda_spec(), check_schema(), profile_missing().
6. Implement or adjust functions until tests pass.
7. Add failing synthetic-data tests.
8. Implement generate_synthetic_data().
9. Add failing summary and plot tests.
10. Implement profile_summaries() and profile_plots().
11. Add failing run_eda() fixture tests.
12. Implement run_eda().
13. Add scurvy fixture later for categorical and small-n testing.
14. Add covid_testing later for larger-data smoke testing.
```

## Guardrails

- Do not use live external downloads in routine tests.
- Do not use private data.
- Do not use generated synthetic data as the only test case.
- Do not let expected outputs be produced by the code under test.
- Do not make optional fixture sources hard package dependencies.
- Keep first fixture small and readable.
