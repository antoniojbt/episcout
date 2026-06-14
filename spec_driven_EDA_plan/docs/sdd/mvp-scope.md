# MVP scope

## Goal

Deliver the smallest useful specification-first EDA workflow on top of `episcout`.

The MVP should prove that a project can be prepared before data access and then rerun unchanged once real data arrive.

## TDD requirement

The MVP must be developed against external real-data fixtures.

Before implementing new spec-first EDA functions, add fixture-backed tests using:

- fixture data;
- fixture specification;
- independently computed expected outputs.

The first fixture should be based on `medicaldata::blood_storage`.

Synthetic data tests are necessary but not sufficient. Synthetic data should not be the only test source for the workflow.

## Included

### Specification

- read CSV data dictionary;
- validate required columns;
- validate allowed variable types;
- support missing-value code declarations;
- support valid ranges and allowed levels;
- return a standard tibble-like object.

### Synthetic data

- generate synthetic data from the specification;
- support numeric, integer, categorical, binary, date, datetime and text variables;
- set deterministic seed;
- clearly label synthetic outputs;
- provide enough realism to test code, plots and reports.

### Schema checks

- compare expected variables with observed variables;
- flag missing variables;
- flag unexpected variables;
- optionally flag type mismatches;
- return machine-readable output.

### Missingness

- missing count per variable;
- missing percentage per variable;
- non-missing count;
- distinct non-missing value count;
- optional row-level missingness.

### Summaries

- numeric summaries: n, missing, mean, SD, median, quartiles, min, max, IQR, outlier counts;
- categorical summaries: n, missing, number of levels, top levels;
- character summaries: missing, empty strings, whitespace strings, unique values;
- date summaries: min, max, missing, observed date range.

### Plots

- numeric: histogram;
- categorical/binary: bar plot;
- date: date-frequency plot;
- text: no default plot in MVP unless low-cardinality;
- one plot per variable;
- save plots as files and/or return plot objects.

### Reporting

- HTML Quarto report first;
- sections for schema, missingness, summaries and plots;
- clear synthetic vs real data labelling;
- no expensive recomputation inside the report.

### Project template

Provide a basic project scaffold:

```text
metadata/data_dictionary.csv
config/eda.yml
_targets.R
reports/eda.qmd
R/project-derivations.R
outputs/
```

## Excluded from MVP

- CRAN release;
- full Arrow backend;
- full DuckDB backend;
- full data.table backend;
- disclosure-control synthetic data;
- causal modelling;
- prediction modelling;
- final manuscript tables;
- complex longitudinal simulation;
- imputation;
- interactive Shiny dashboard;
- PDF/Word report output.

## Risk controls

- keep public API small;
- add tests with every new function;
- do not refactor unrelated existing functions;
- preserve existing `epi_*` functions initially;
- avoid dependency bloat;
- place heavy packages in `Suggests` where possible.

## Development order

Use the TDD-first order in `spec_driven_EDA_plan/docs/codex/revised-pr-plan-tdd-first.md`.

In short:

1. Add external fixtures and independently computed expected outputs.
2. Add failing fixture-backed tests.
3. Implement the smallest function set needed to pass those tests.
4. Repeat test-first for each workflow layer.
