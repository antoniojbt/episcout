# Repository audit note

## Summary

`episcout` is a suitable foundation for a specification-first EDA workflow, but it currently behaves more like a collection of epidemiology helper functions than an orchestrated workflow package.

The package should not be rewritten from scratch. The preferred approach is to keep existing `epi_*` helpers and add a higher-level specification-first layer.

## Current strengths

- Domain fit: epidemiology and biomedical data.
- Existing cleaning, plotting, statistics and utility functions.
- Existing tests across several function families.
- Existing package structure with roxygen2 documentation.
- Existing `AGENTS.MD` conventions for Codex-style development.
- Useful descriptive-statistics functions already present.

## Current limitations

- No specification-first workflow.
- No data dictionary parser as central project contract.
- No synthetic-data generator driven by expected variable metadata.
- No single high-level `run_eda()` orchestration function.
- No standard EDA report template.
- No project template for reproducible analysis.
- No large-data backend abstraction.
- Broad public API with many exported helper functions.

## Architectural recommendation

Add a new layer rather than replacing existing functions.

```text
Existing layer:
- epi_clean_*
- epi_stats_*
- epi_plot_*
- epi_utils_*

New layer:
- eda_spec()
- validate_eda_spec()
- generate_synthetic_data()
- check_schema()
- profile_missing()
- profile_summaries()
- profile_plots()
- run_eda()
- render_eda_report()
```

## Immediate code review targets

### `epi_stats_numeric()`

Review:

- all-missing vectors;
- zero-length vectors;
- `min()`/`max()` behaviour with all missing values;
- coefficient of variation when mean is zero;
- Shapiro-Wilk handling with missing values;
- repeated calculation of SD and mean.

### `epi_stats_na_perc()`

Review:

- use of `apply()` on data frames;
- mixed-type data frame coercion;
- row names in output;
- consistent tibble output;
- large-data performance.

### `epi_stats_summary()`

Review:

- use of `expression()` and `eval()`;
- use of older `select_if()`;
- clearer dispatch by variable type;
- more explicit input validation.

### Plotting functions

Review:

- optional dependencies;
- consistent theme behaviour;
- whether functions return ggplot objects without printing;
- automatic plot dispatch by variable type.

## Dependency policy

Suggested direction:

Hard dependencies should be minimal and justified.

Potential hard dependencies:

- `rlang`
- `tibble`
- `dplyr`
- `purrr`
- `ggplot2`

Potential suggested dependencies:

- `data.table`
- `arrow`
- `duckdb`
- `quarto`
- `targets`
- `skimr`
- `naniar`
- `visdat`
- `pointblank`
- `validate`
- `simstudy`

Avoid adding large dependencies to `Imports` unless required for core functionality.

## Backwards compatibility

Do not remove or rename existing exported `epi_*` functions in the MVP.

If deprecation becomes necessary later, use a staged deprecation plan:

1. document replacement;
2. warn;
3. soft-deprecate;
4. remove only in a major version.
