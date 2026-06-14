# Roadmap

## Phase 0: Documentation and baseline

Deliverables:

- `docs/sdd/0001-spec-first-eda.md`
- ADR index
- roadmap
- Codex instructions
- PR plan

No package behaviour changes.

## Phase 1: Stabilise current package

Goals:

- run `R CMD check`;
- run test suite;
- record baseline coverage;
- identify fragile functions;
- fix clear edge cases.

Priority functions:

- `epi_stats_numeric()`
- `epi_stats_na_perc()`
- `epi_stats_summary()`
- `epi_stats_chars()`
- `epi_stats_factors()`
- `epi_plot_*()`

Known concerns to review:

- broad public API;
- accidental exports;
- edge cases with all-missing numeric vectors;
- coefficient of variation when mean is zero;
- mixed-type data frames in `apply()`;
- older tidyverse idioms such as `select_if()`;
- functions that require suggested packages at runtime.

## Phase 2: TDD fixtures for spec-first EDA

Start with fixture-backed tests before implementing the new spec-first API.

Deliverables:

- add the external fixture plan;
- add the `medicaldata::blood_storage` fixture;
- add the fixture data dictionary;
- add independently computed expected schema and missingness outputs;
- add failing fixture-backed tests.

No new EDA API should be implemented before the first fixture-backed tests are in place.

## Phase 3: Add specification-first MVP

New files:

```text
R/eda_spec.R
R/eda_schema.R
R/eda_synthetic.R
R/eda_missing.R
R/eda_summaries.R
R/eda_plots.R
R/eda_report.R
R/run_eda.R
```

Initial public functions:

```r
eda_spec()
validate_eda_spec()
generate_synthetic_data()
check_schema()
profile_missing()
profile_summaries()
profile_plots()
run_eda()
render_eda_report()
```

## Phase 4: Reporting and project template

Add:

```text
inst/project-template/
inst/report-template/
```

Template contents:

```text
metadata/data_dictionary.csv
config/eda.yml
_targets.R
reports/eda.qmd
R/project-derivations.R
outputs/
```

Output format:

- HTML first;
- Word/PDF later if needed.

## Phase 5: Large-data optimisation

Sequence:

1. data.frame/tibble first;
2. data.table backend;
3. DuckDB backend;
4. Arrow backend;
5. approximate plotting and cached full summaries.

Design principle:

- collect small samples for plotting;
- compute summaries on full data;
- cache machine-readable outputs;
- do not recompute inside reports.

## Phase 6: Biomedical extensions

Potential extensions:

- missingness by site;
- missingness by exposure/outcome;
- missingness by calendar time;
- complete-case counts for analysis variable sets;
- longitudinal summaries;
- event-rate summaries;
- survival-time checks;
- implausible clinical-value checks;
- variable role-based report sections.
