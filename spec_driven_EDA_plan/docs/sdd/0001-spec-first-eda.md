# SDD 0001: Specification-first exploratory data analysis

## Status

Draft.

## Objective

Evolve `episcout` from a collection of epidemiology helper functions into a specification-driven exploratory data analysis workflow for biomedical and epidemiological datasets.

The package should support two common project states:

1. Real data are already available.
2. Only variable names, expected metadata, or a draft data dictionary are available while data access is pending.

In both cases, the same project specification should drive schema checks, missingness summaries, descriptive statistics, automated plots, synthetic data generation and reproducible reports.

## Rationale

Biomedical datasets often require substantial first-pass review before formal analysis. Recurring early tasks include:

- checking expected and observed variable names;
- checking data types;
- standardising missing-value codes;
- summarising missingness;
- summarising numeric, categorical, date and text variables;
- detecting impossible or suspicious values;
- generating one plot per variable;
- documenting outputs for review;
- preparing project-specific derived-variable code.

These tasks are often repeated manually across projects. They are also often delayed when real data access is slow because governance, permissions or data-provider processes are incomplete.

A specification-first workflow allows teams to prepare project code before data access, then rerun the same workflow on real data once available. The specification acts as a contract between expected data structure and observed data.

## Intended users

Primary users:

- epidemiologists;
- clinical researchers;
- biostatisticians;
- data scientists;
- research software engineers;
- analysts working with biomedical datasets.

The package should be useful for internal research teams handling medium to large datasets, including data with many variables, site-specific coding, non-standard missing-value codes and project-specific derived variables.

## Core idea

The central object is an EDA specification, usually created from a data dictionary.

The specification should include, at minimum:

- variable name;
- label;
- expected type;
- analytical role;
- units;
- allowed levels;
- valid range;
- missing-value codes;
- optional derivation notes;
- optional grouping or stratification variables.

The same specification should be used to:

- generate synthetic data;
- validate real data;
- standardise missing values;
- run summaries;
- generate plots;
- render reports.

## Target workflows

### Workflow A: real data already available

```r
spec <- epi_eda_spec("metadata/data_dictionary.csv")
data <- read_project_data("data/raw/data.parquet")

epi_eda_run(
  data = data,
  spec = spec,
  out_dir = "outputs/eda_real",
  mode = "real"
)
```

Expected outputs:

- schema mismatch report;
- missingness profile;
- summary statistics;
- plots per variable;
- range and level violations;
- Quarto report;
- machine-readable output files.

### Workflow B: data access pending

```r
spec <- epi_eda_spec("metadata/data_dictionary.csv")
data <- epi_eda_generate_synthetic_data(spec, n = 10000, seed = 1)

epi_eda_run(
  data = data,
  spec = spec,
  out_dir = "outputs/eda_synthetic",
  mode = "synthetic"
)
```

Expected outputs:

- same report structure as the real-data workflow;
- same plotting and summary code;
- tested derived-variable code;
- clearly labelled synthetic outputs;
- no inferential interpretation.

When real data arrive, only the data source should change.

## MVP scope

The first implementation should include:

1. Read and validate an EDA specification.
2. Generate simple synthetic data from the specification.
3. Check expected variables against observed variables.
4. Standardise common missing-value codes.
5. Summarise missingness by variable.
6. Produce descriptive summaries by variable type.
7. Produce one basic plot per variable.
8. Save machine-readable outputs.
9. Render a Quarto EDA report.
10. Provide a project template.

## Non-goals

The package should not replace:

- project-specific modelling;
- causal inference;
- prediction modelling;
- phenotyping;
- final publication tables;
- disclosure-control synthetic data generation.

Synthetic data are for pipeline preparation and testing only. They are not for inference or external data release.

The package should not immediately optimise every operation for very large data. The first version should establish a stable API, tests and report structure. Large-data backends can follow.

## Proposed public API

Initial public functions:

```r
epi_eda_spec()
epi_eda_validate_spec()
epi_eda_generate_synthetic_data()
epi_eda_check_schema()
epi_eda_profile_missing()
epi_eda_profile_summaries()
epi_eda_profile_plots()
epi_eda_run()
epi_eda_render_report()
```

Existing `epi_*` functions should remain available initially. Over time, they may be refactored into lower-level helpers if this can be done without breaking existing users.

## Package layers

### Layer 1: existing helper functions

Examples:

- `epi_stats_numeric()`
- `epi_stats_chars()`
- `epi_stats_factors()`
- `epi_stats_na_perc()`
- `epi_plot_*()`
- `epi_clean_*()`

These remain useful as lower-level operations.

### Layer 2: new specification-first workflow

Examples:

- read variable specification;
- generate synthetic data;
- validate real data against specification;
- run missingness, summaries and plots;
- save machine-readable outputs;
- render Quarto report;
- provide project template.

## Design principles

The package should favour:

- small composable functions;
- explicit inputs;
- deterministic outputs;
- clear errors;
- testable return objects;
- backwards compatibility where practical;
- plain R objects as outputs;
- reports that consume saved outputs rather than recomputing expensive steps.

The new spec-first EDA layer should be developed test-first against external real-data fixtures where practical. Expected test outputs should be independently computed and should not be generated by the functions under test.

Outputs should usually be:

- tibbles;
- lists;
- ggplot objects;
- files saved under `outputs/`.

## Large-data direction

The MVP should work with ordinary data frames and tibbles.

Later versions should support:

- `data.table` for fast in-memory aggregation;
- Arrow for multi-file Parquet datasets;
- DuckDB for SQL-style local analysis;
- sampled data for plotting;
- full-data aggregation for summaries;
- cached intermediate outputs.

Avoid premature optimisation. Design the API so backends can be added without breaking users.

## Definition of done for MVP

The MVP is complete when a user can:

1. create a data dictionary;
2. generate synthetic data;
3. run the full EDA workflow;
4. render a report;
5. replace synthetic data with real data;
6. rerun the same workflow without changing analysis code apart from the data source.
