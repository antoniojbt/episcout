# Revised Codex PR plan: TDD-first order

## Purpose

This is the stable PR sequence for the specification-first EDA layer.

This file is not the live status dashboard. Use
`archive/eda_sdd_tdd_r1_archive/START_HERE.md` for current status, the active PR,
allowed edits, expected test state and closeout requirements.

The rule is:

```text
External fixture and expected output first.
Failing fixture-backed tests second.
Implementation third.
```

This prevents Codex from writing code first and then writing tests that only confirm its own implementation.

## PR sequence

### PR 1: Add external fixture files

Scope:

- add real-data fixture files;
- add a manually reviewed fixture specification;
- add independently computed expected schema and missingness outputs.

Files:

```text
data-raw/test-fixtures/make_external_fixtures.R
tests/testthat/fixtures/README.md
tests/testthat/fixtures/blood_storage/SOURCE.md
tests/testthat/fixtures/blood_storage/blood_storage.csv
tests/testthat/fixtures/blood_storage/blood_storage_spec.csv
tests/testthat/fixtures/blood_storage/expected_schema.csv
tests/testthat/fixtures/blood_storage/expected_missing.csv
```

No new package behaviour.

### PR 2: Add failing tests for specification, schema and missingness

Scope:

- add tests that use the blood_storage fixture;
- do not implement the new functions yet.
- this PR may intentionally leave CI red because the functions under test do
  not exist yet.

Files:

```text
tests/testthat/test-epi_eda_spec-fixtures.R
tests/testthat/test-eda_schema-fixtures.R
tests/testthat/test-eda_missing-fixtures.R
```

Expected state:

- tests may fail because functions are not implemented yet;
- PR 3 is expected to restore passing tests by implementing the missing
  functions.

### PR 3: Implement specification, schema and missingness functions

Scope:

- implement only the functions needed to pass PR 2 tests.

Files:

```text
R/epi_eda_spec.R
R/eda_schema.R
R/eda_missing.R
```

Functions:

```r
epi_eda_spec()
epi_eda_validate_spec()
epi_eda_check_schema()
epi_eda_profile_missing()
```

### PR 4: Add failing synthetic-data tests

Scope:

- add fixture-backed tests for synthetic data generation;
- do not implement `epi_eda_generate_synthetic_data()` yet.

File:

```text
tests/testthat/test-eda_synthetic-fixtures.R
```

Tests should use:

```text
tests/testthat/fixtures/blood_storage/blood_storage_spec.csv
```

### PR 5: Implement synthetic-data generation

Scope:

- implement only synthetic-data generation.

File:

```text
R/eda_synthetic.R
```

Function:

```r
epi_eda_generate_synthetic_data()
```

### PR 6: Add failing summary and plot tests

Scope:

- add fixture-backed tests for summaries and plots;
- do not implement `epi_eda_profile_summaries()` or `epi_eda_profile_plots()` yet.

Files:

```text
tests/testthat/test-eda_summaries-fixtures.R
tests/testthat/test-eda_plots-fixtures.R
```

Optional expected outputs:

```text
tests/testthat/fixtures/blood_storage/expected_summary_numeric.csv
tests/testthat/fixtures/blood_storage/expected_summary_categorical.csv
```

### PR 7: Implement summaries and plot dispatch

Scope:

- implement summary profiling and plot dispatch.

Files:

```text
R/eda_summaries.R
R/eda_plots.R
```

Functions:

```r
epi_eda_profile_summaries()
epi_eda_profile_plots()
```

### PR 8: Add failing epi_eda_run end-to-end tests

Scope:

- add fixture-backed end-to-end workflow tests;
- do not implement `epi_eda_run()` yet.

File:

```text
tests/testthat/test-epi_eda_run-fixtures.R
```

Tests:

- real fixture data workflow;
- synthetic data workflow;
- temporary output directory;
- expected named components.

### PR 9: Implement epi_eda_run()

Scope:

- implement the orchestration function.

File:

```text
R/epi_eda_run.R
```

Function:

```r
epi_eda_run()
```

### PR 10: Add report-template tests

Scope:

- add tests for report rendering;
- do not implement `epi_eda_render_report()` yet.

File:

```text
tests/testthat/test-eda_report.R
```

### PR 11: Implement report rendering

Scope:

- add Quarto report template and rendering function.

Files:

```text
R/eda_report.R
inst/report-template/eda.qmd
```

Function:

```r
epi_eda_render_report()
```

### PR 12: Add project-template contract tests

Scope:

- add failing tests for the reusable project scaffold;
- add failing tests for the user-facing project setup helper;
- do not add the template or implement the helper yet.

Files:

```text
tests/testthat/test-project-template.R
```

Future function contract:

```r
epi_eda_create_project(path, overwrite = FALSE)
```

Expected template entries:

```text
metadata/data_dictionary.csv
config/eda.yml
_targets.R
reports/eda.qmd
R/project-derivations.R
outputs/
```

Expected state:

- tests may fail because `inst/project-template/` and
  `epi_eda_create_project()` are not implemented yet;
- PR 13 is expected to restore passing tests by implementing the missing
  scaffold and helper.

### PR 13: Implement project template

Scope:

- add reusable project scaffold;
- implement the user-facing project setup helper.

Files:

```text
inst/project-template/
R/epi_eda_create_project.R
```

Function:

```r
epi_eda_create_project(path, overwrite = FALSE)
```

### PR 14: Add large-data design note

Scope:

- document large-data backend strategy.

Do not fully implement Arrow, DuckDB or data.table backends yet unless explicitly requested.

## Principle for all implementation PRs

Every new function should have a fixture-backed test when practical.

Expected outputs must be independently computed from fixture data and the data dictionary, not generated by the function under test.
