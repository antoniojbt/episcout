# Software Design

Spec ID: `008-univariate-stats-eda-alignment`  
Status: Implemented  

## Scope

Implement common missing/count and numeric, categorical, text and temporal cores; adapt active public univariate statistics functions; add typed `epi_stats_summary()` output; and add versioned EDA summaries, CSV outputs and report sections.

## Public API

- Add `summary_version = c("v1", "v2")` to `epi_eda_profile_summaries()`, `epi_eda_run()` and `epi_eda_render_report()`.
- Keep `v1` as the default with its exact two summary components and output files.
- Add `output = c("current", "typed")` to `epi_stats_summary()`; `current` retains existing behaviour and `typed` returns the six v2 summary components.
- Preserve the exported names and output schemas of the existing numeric, outlier, missingness, character, factor and temporal wrappers.

## Inputs And Outputs

EDA v2 returns `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped`. `variables` records each specification row, counts, status and reason. Numeric output records finite-value descriptive statistics, shape, normality and Tukey outliers. Categorical output records declared and observed levels with both denominators. Text output records missingness, cardinality and length/whitespace metrics. Temporal output records source class/timezone, ISO values and range units. `skipped` records missing variables, incompatible classes and invalid temporal values.

Typed `epi_stats_summary()` infers types from observed classes, treats `codes` as global sentinels, uses variable names as labels and leaves roles unavailable. It requires `action = "exclude"`; non-default legacy mode requests fail clearly.

## Data Flow

1. Validate the requested output version/mode and inputs.
2. Construct missing masks, with EDA applying per-variable specification codes and typed statistics applying global codes.
3. Dispatch each variable to one unexported type core.
4. Adapt core outputs to either historical public wrapper schemas, EDA v1 schemas or v2 component schemas.
5. Write deterministic machine-readable outputs and render the matching report sections.

## Edge Policies

- `NA`, `NaN` and applicable sentinel codes are missing.
- Numeric infinities are counted but excluded from calculations.
- Empty and all-missing inputs return stable typed rows; zero denominators return `NA`.
- Numeric shape and normality fields are `NA` when sample size or variation is insufficient.
- Declared categorical levels remain with zero counts; unexpected observed levels are appended and identified; literal `"NA"` remains distinct from missing.
- Text lengths use raw strings; empty strings and non-empty whitespace-only strings are distinct.
- Date inputs accept `Date`, `IDate` and ISO `YYYY-MM-DD` character values and report ranges in days.
- Datetime inputs accept `POSIXct`, `POSIXlt` and ISO-8601 character values, report ISO UTC values with source timezone metadata, and report ranges in seconds.
- Invalid non-missing temporal values and incompatible classes produce an explicit skip.
- Non-syntactic names are accessed by exact name.

## Compatibility

EDA v1 remains byte-schema compatible and does not add metadata columns or extra files. EDA v2 writes `summary_variables.csv`, `summary_numeric.csv`, `summary_categorical.csv`, `summary_text.csv`, `summary_temporal.csv` and `summary_skipped.csv`. Existing public wrapper column names and ordering remain unchanged; only confirmed incorrect edge calculations change.

## Dependencies

No new dependency is introduced. Existing base R, `e1071`, tidyverse imports and optional report tooling remain in use.
