# Idea: EDA and Stats Helper Boundaries

Status: Idea to consider  
Suggested spec ID: `005-eda-stats-helper-boundaries`  
Date captured: 2026-06-15

## Summary

The current `epi_eda_*` workflow mostly does not call `epi_stats_*` helpers.
That is not automatically a problem: the EDA workflow is specification-first
and returns smaller, machine-readable outputs, while the older `epi_stats_*`
helpers expose broader descriptive-statistics APIs.

The risk is duplicated edge-case logic. Numeric summaries and missingness are
computed directly in the EDA layer, even though similar behaviours exist in
`epi_stats_numeric()` and `epi_stats_na_perc()`. Future work should decide
whether to share small internal helper functions rather than make EDA depend
directly on the full exported `epi_stats_*` APIs.

## Checks Performed

Commands used for inspection:

```bash
rg --files R tests DESCRIPTION NAMESPACE future | sort
rg -n "epi_eda_|epi_stats_|@import|::|library\\(|require\\(|Imports|Suggests" R DESCRIPTION NAMESPACE tests future
sed -n '1,220p' R/eda_summaries.R
sed -n '1,220p' R/eda_missing.R
sed -n '1,240p' R/eda_schema.R
sed -n '1,220p' R/eda_plots.R
sed -n '1,260p' R/eda_spec.R
sed -n '1,260p' R/run_eda.R
sed -n '1,280p' R/eda_report.R
sed -n '1,260p' R/eda_synthetic.R
sed -n '1,140p' DESCRIPTION
sed -n '1,230p' R/epi_stats_numeric.R
sed -n '1,180p' R/epi_stats_na_perc.R
sed -n '1,160p' R/epi_stats_summary.R
sed -n '1,150p' tests/testthat/test-eda_summaries-fixtures.R
```

Observed call structure:

- `epi_eda_run()` orchestrates `epi_eda_check_schema()`,
  `epi_eda_profile_missing()`, `epi_eda_profile_summaries()` and
  `epi_eda_profile_plots()`.
- `epi_eda_profile_summaries()` calls `epi_eda_spec()` and internal helpers
  `profile_summaries_numeric()` and `profile_summaries_categorical()`.
- `profile_summaries_numeric()` computes `n`, `n_missing`, `mean`, `sd`,
  `median`, `min` and `max` directly using base R and `stats`.
- `epi_eda_profile_missing()` computes missingness directly with
  `sum(is.na(...))`.
- `epi_eda_profile_plots()` uses `requireNamespace("ggplot2")` and then
  `ggplot2::` calls.
- `epi_eda_render_report()` uses `requireNamespace("rmarkdown")` and
  `rmarkdown::render()`.
- No inspected `epi_eda_*` function directly calls `epi_stats_numeric()`,
  `epi_stats_na_perc()`, `epi_stats_summary()` or another exported
  `epi_stats_*` summary helper.

## Why This Exists

Direct reuse of `epi_stats_numeric()` inside EDA is not obviously correct.
`epi_stats_numeric()` returns a wide, rich statistics row with quantiles, IQR,
CV, variance, SEM, skewness, kurtosis, Shapiro-Wilk p-value and Tukey outlier
fields. The EDA summary contract is much smaller and tied to a data dictionary:
`name`, `n`, `n_missing`, `mean`, `sd`, `median`, `min`, `max`.

Direct reuse of `epi_stats_na_perc()` also is not a perfect fit. Its output is
row- or column-indexed and uses `na_counts` and `na_perc`, while
`epi_eda_profile_missing()` returns one row per spec variable with `name`, `n`,
`n_missing` and `p_missing`.

So the recommended direction is not "make EDA call every matching stats
function". The better direction is to share narrow internal primitives for
behaviour that must stay consistent, especially safe scalar summaries and
missing-proportion handling.

## Where To Look

Primary EDA files:

- `R/run_eda.R`: EDA workflow orchestration and output writing.
- `R/eda_summaries.R`: numeric and categorical EDA summary logic.
- `R/eda_missing.R`: EDA missingness summary logic.
- `R/eda_plots.R`: EDA plot generation and `ggplot2` dependency guard.
- `R/eda_report.R`: report rendering and `rmarkdown` dependency guard.
- `R/eda_spec.R`: spec parsing and validation.
- `R/eda_synthetic.R`: synthetic data generation from specs.

Related stats files:

- `R/epi_stats_numeric.R`: richer numeric summaries and safer empty/all-missing
  handling.
- `R/epi_stats_na_perc.R`: row/column missingness summaries.
- `R/epi_stats_summary.R`: older multi-column summary wrapper that already
  delegates to `epi_stats_numeric()` for numeric columns in one path.

Related tests:

- `tests/testthat/test-eda_summaries-fixtures.R`
- `tests/testthat/test-eda_missing-fixtures.R`
- `tests/testthat/test-run_eda-fixtures.R`
- `tests/testthat/test-epi_stats_numeric.R`
- `tests/testthat/test-stats.R`

## Future SDD Construction

Create a numbered spec only if this becomes active work. The SDD should make
these decisions explicit:

- Goal: align EDA and stats edge-case behaviour without changing public
  `epi_eda_*` or `epi_stats_*` return contracts unless a specific bug requires
  it.
- Preferred implementation: extract small unexported helper(s) for safe numeric
  scalar summaries and missing proportions, then call them from both relevant
  EDA and stats code where it reduces duplication.
- Avoid direct dependency from EDA summaries to the full `epi_stats_numeric()`
  output unless the EDA output schema is intentionally expanded.
- Preserve existing EDA CSV output column names written by `epi_eda_run()`.
- Preserve existing `epi_stats_numeric()` and `epi_stats_na_perc()` public
  argument names and documented return shapes.
- Do not add new package dependencies.
- Keep changes scoped to summary/missingness behaviour; plotting, schema
  validation, synthetic data and report rendering should remain out of scope
  unless tests reveal a directly related issue.

Suggested SDD sections:

- Scope: EDA numeric summaries, EDA missingness and shared internal helper
  behaviour.
- Desired behaviour: empty, all-missing, mixed missing/non-missing, zero-row and
  non-numeric edge cases.
- Compatibility: explicit list of preserved public output columns and any
  intentional bug-fix changes.
- Out of scope: broad EDA redesign, richer statistical summaries, plotting
  redesign and dependency changes.

## Future TDD Construction

The TDD should start from executable tests before package-code changes.

Baseline commands:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```

Focused test files:

- Add or extend `tests/testthat/test-eda_summaries-fixtures.R` for EDA summary
  edge cases.
- Add or extend `tests/testthat/test-eda_missing-fixtures.R` for missingness
  edge cases.
- Extend `tests/testthat/test-epi_stats_numeric.R` and
  `tests/testthat/test-stats.R` only if shared helper changes affect stats
  behaviour.

Required test scenarios:

- All-missing numeric EDA variable returns finite-compatible missing summary
  values, preferably `NA_real_` for unavailable statistics rather than `NaN`,
  `Inf` or `-Inf`.
- Zero-row EDA data returns stable summary and missingness shapes.
- Mixed missing/non-missing numeric variables keep existing valid calculations.
- EDA missingness keeps `name`, `n`, `n_missing` and `p_missing` columns.
- Stats helper outputs keep their current public column names.
- Shared helper changes do not introduce `ggplot2`, `rmarkdown` or tidyverse
  requirements into non-plot/non-report EDA paths.

Acceptance commands:

```bash
scripts/rscript_env_caller.R -e "devtools::load_all(); testthat::test_file('tests/testthat/test-eda_summaries-fixtures.R')"
scripts/rscript_env_caller.R -e "devtools::load_all(); testthat::test_file('tests/testthat/test-eda_missing-fixtures.R')"
scripts/rscript_env_caller.R -e "devtools::load_all(); testthat::test_file('tests/testthat/test-epi_stats_numeric.R')"
scripts/rscript_env_caller.R -e "devtools::load_all(); testthat::test_file('tests/testthat/test-stats.R')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
```

## Machine-Readable Handoff

```yaml
idea_id: eda-stats-helper-boundaries
status: idea
suggested_spec_id: 005-eda-stats-helper-boundaries
primary_question: Should EDA and stats share narrow internal helpers for duplicated summary and missingness edge-case behaviour?
recommendation: Extract small unexported primitives if future tests show duplicated behaviour must stay aligned; do not make EDA directly depend on full exported epi_stats_* output schemas.
primary_files:
  - R/eda_summaries.R
  - R/eda_missing.R
  - R/run_eda.R
  - R/epi_stats_numeric.R
  - R/epi_stats_na_perc.R
primary_tests:
  - tests/testthat/test-eda_summaries-fixtures.R
  - tests/testthat/test-eda_missing-fixtures.R
  - tests/testthat/test-epi_stats_numeric.R
  - tests/testthat/test-stats.R
public_contracts_to_preserve:
  - epi_eda_profile_summaries return list names: numeric, categorical
  - EDA numeric summary columns: name, n, n_missing, mean, sd, median, min, max
  - EDA missingness columns: name, n, n_missing, p_missing
  - epi_stats_numeric public arguments and return columns
  - epi_stats_na_perc public arguments and return columns
out_of_scope:
  - plotting redesign
  - report rendering redesign
  - synthetic data redesign
  - new dependencies
```
