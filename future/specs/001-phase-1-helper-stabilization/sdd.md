# Software Design

Spec ID: `001-phase-1-helper-stabilization`  
Status: Implemented

## Scope

Harden `epi_stats_numeric()` and `epi_stats_na_perc()` while preserving existing
function names, argument names and data-frame-like return values where
practical.

## Desired Behaviour

- `epi_stats_numeric()` handles all-missing numeric vectors deterministically.
- `epi_stats_numeric()` handles zero-length numeric vectors deterministically.
- Coefficient of variation is stable when the mean is zero.
- Normality, skewness and kurtosis are skipped safely when usable values are
  insufficient.
- `epi_stats_na_perc()` rejects invalid `margin` values with a clear error.
- `epi_stats_na_perc()` counts missing values in mixed-type data frames without
  matrix coercion changing values.
- Row and column modes keep stable return shapes.

## Compatibility

Preserve backwards compatibility unless an old behaviour is clearly erroneous
and now covered by tests. Document intentional behaviour changes in tests and
roxygen comments.

## Out Of Scope

- Redesigning the `epi_stats_*` API.
- Updating every superseded tidyverse idiom.
- Editing `R/eda_*`, `R/run_eda.R` or report/project templates.
- Adding new dependencies.
