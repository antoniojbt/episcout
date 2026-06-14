# Revised Codex PR plan: TDD-first order

## Purpose

This replaces the earlier implementation-first order for the new specification-first EDA layer.

The new rule is:

```text
External fixture and expected output first.
Failing fixture-backed tests second.
Implementation third.
```

This prevents Codex from writing code first and then writing tests that only confirm its own implementation.

## Revised PR sequence

### PR 1: Documentation baseline

Scope:

- add or review SDD, ADRs, roadmap and Codex instructions;
- no R code changes.

Files:

```text
docs/sdd/0001-spec-first-eda.md
docs/sdd/mvp-scope.md
docs/sdd/data-dictionary-spec.md
docs/roadmap.md
docs/repository-audit.md
docs/adr/*
docs/codex/*
```

### PR 2: Add TDD external-fixture plan

Scope:

- add this TDD fixture plan;
- document revised order.

Files:

```text
docs/sdd/tdd-external-fixtures.md
docs/codex/revised-pr-plan-tdd-first.md
docs/codex/tdd-first-codex-instructions.md
docs/codex/update-existing-docs.md
docs/adr/0005-external-fixture-tdd.md
```

No R code changes.

### PR 3: Baseline checks

Scope:

- run existing checks;
- record current package state.

Possible file:

```text
docs/baseline-checks.md
```

Do not add new EDA API yet.

### PR 4: Add external fixture files

Scope:

- add real-data fixture;
- add manually reviewed specification;
- add independently computed expected outputs.

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

### PR 5: Add failing tests for specification, schema and missingness

Scope:

- add tests that use the blood_storage fixture.

Files:

```text
tests/testthat/test-eda_spec-fixtures.R
tests/testthat/test-eda_schema-fixtures.R
tests/testthat/test-eda_missing-fixtures.R
```

Expected state:

- tests may fail because functions are not implemented yet.

### PR 6: Implement specification, schema and missingness functions

Scope:

- implement only the functions needed to pass PR 5 tests.

Files:

```text
R/eda_spec.R
R/eda_schema.R
R/eda_missing.R
```

Functions:

```r
eda_spec()
validate_eda_spec()
check_schema()
profile_missing()
```

### PR 7: Add failing synthetic-data tests

Scope:

- add fixture-backed tests for synthetic data generation.

File:

```text
tests/testthat/test-eda_synthetic-fixtures.R
```

Tests should use:

```text
tests/testthat/fixtures/blood_storage/blood_storage_spec.csv
```

Expected state:

- tests may fail because `generate_synthetic_data()` is not implemented yet.

### PR 8: Implement synthetic-data generation

Scope:

- implement only synthetic-data generation.

File:

```text
R/eda_synthetic.R
```

Function:

```r
generate_synthetic_data()
```

### PR 9: Add failing summary and plot tests

Scope:

- add fixture-backed tests for summaries and plots.

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

### PR 10: Implement summaries and plot dispatch

Scope:

- implement summary profiling and plot dispatch.

Files:

```text
R/eda_summaries.R
R/eda_plots.R
```

Functions:

```r
profile_summaries()
profile_plots()
```

### PR 11: Add failing run_eda end-to-end tests

Scope:

- add fixture-backed end-to-end workflow tests.

File:

```text
tests/testthat/test-run_eda-fixtures.R
```

Tests:

- real fixture data workflow;
- synthetic data workflow;
- temporary output directory;
- expected named components.

### PR 12: Implement run_eda()

Scope:

- implement the orchestration function.

File:

```text
R/run_eda.R
```

Function:

```r
run_eda()
```

### PR 13: Add report-template tests

Scope:

- add tests for report rendering.

File:

```text
tests/testthat/test-eda_report.R
```

Expected state:

- tests may fail before implementation.

### PR 14: Implement report rendering

Scope:

- add Quarto report template and rendering function.

Files:

```text
R/eda_report.R
inst/report-template/eda.qmd
```

Function:

```r
render_eda_report()
```

### PR 15: Add project template

Scope:

- add reusable project scaffold.

Files:

```text
inst/project-template/
```

Optional function:

```r
use_episcout_project()
```

### PR 16: Add large-data design note

Scope:

- document large-data backend strategy.

Do not fully implement Arrow, DuckDB or data.table backends yet unless explicitly requested.

## Revised principle for all implementation PRs

Every new function should have a fixture-backed test when practical.

Expected outputs must be independently computed from fixture data and the data dictionary, not generated by the function under test.

## What changed from the earlier order

Earlier order:

```text
eda_spec()
synthetic data
schema/missingness
summaries/plots
run_eda()
report
project template
```

Revised order:

```text
fixture plan
fixture files
failing tests
implementation
repeat function by function
```

The main change is that `eda_spec()` is no longer the first code PR. Fixture and expected-output machinery come first.
