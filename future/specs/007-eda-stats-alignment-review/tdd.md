# Future Test Design

Spec ID: `007-eda-stats-alignment-review`
Status: Completed review; executable tests deferred

## Boundary

This document maps confirmed gaps to behaviour tests for a later implementation spec. Spec 007 does not add or edit executable tests.

## Baseline Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```

## Baseline Results — 2026-07-25

- Full `devtools::test(reporter = "summary")`: passed with two existing skips and no failures or warnings.
- The disabled `vdiffr` harness deleted tracked plotting snapshots during cleanup; the unchanged snapshots were restored immediately, matching the known repository behaviour recorded by spec 006.
- `devtools::check(manual = FALSE)`: 0 errors, 0 warnings and 1 existing NOTE for `.gitkeep` files under `inst/project-template/`; suggested package `targets` was unavailable for checking.
- No package source or test file was changed by this review.

## Future Behaviour-Test Matrix

| Area | Required scenarios | Intended evidence |
| --- | --- | --- |
| Common counts and missingness | Standard `NA`, multiple sentinel codes, mixed missing/observed data, zero rows, missing specification variable and non-syntactic names | Shared counts agree across `epi_stats_*` adapters and EDA outputs; EDA retains specification-aware sentinel handling |
| Numeric/integer | Ordinary values, singleton, all missing, zero length, `na.rm = FALSE`, zero mean, insufficient normality sample, `NaN`, `Inf`, `-Inf`, integer inputs and non-numeric inputs | Stable typed counts and statistics, explicit non-finite policy, no opaque errors or contradictory finite/non-finite results |
| Outliers | Empty/all-missing, zero/negative coefficient, constant vectors, non-finite values and agreement with numeric summary fences | One shared Tukey implementation and consistent count types |
| Categorical/binary | Declared and observed levels, unused declared levels, undeclared observed values, factors, characters, logicals, integer-coded binary values, missing codes and zero rows | Declared zero-count levels retained, unexpected levels surfaced, denominators explicit and stable |
| Text | Missing, empty, whitespace-only, multibyte strings, all missing, zero rows and mixed non-text columns | One row per text variable with stable length/count fields and explicit coercion policy |
| Date/datetime | Date, IDate, POSIXct, POSIXlt, time zones, all missing, zero length, duplicates and date ranges | Stable temporal schema, preserved class/time zone and documented range units |
| Data-frame summary orchestration | Mixed seven-type data, codes included/excluded, no eligible columns, invalid class/action and deterministic variable order | Dispatches to shared cores without expression/eval, returns one documented composite contract |
| Presentation helpers | Zero denominator, missing ordering column, numeric skip selection, large/small numbers and class preservation | Formatting does not silently alter computational values; invalid parameters fail clearly |
| Correlation | Pearson/Spearman, invalid method, non-numeric columns, fewer than five observations, constant columns, missing pairs, empty inputs and non-syntactic names | Validated inputs, stable matrix/long outputs and explicit undefined-correlation representation |
| Correlation reshape/labels | Missing or malformed correlation object, empty triangles, label-length mismatch and omitted arguments | Required arguments have valid defaults or are required; output columns remain consistent with plotting consumers |
| 2x2 contingency | Missing variables, `NA` levels, exactly two levels, more than two levels, sparse cells, invalid test type and fixed randomness policy | Clear eligibility/test-selection semantics, reproducible results and actionable errors |
| NxN contingency | Arbitrary outcome labels, empty and single-level data, zero totals, multiple independent variables and non-syntactic names | No hard-coded `Yes`/`No` columns, valid percentages and stable long/wide contract |
| Outcome proportion | Window-specific numerator, missing values, non-binary outcome, absent variables, zero denominator and multiple windows | Numerator and denominator use the same population window; no printing side effect; zero denominator handled explicitly |
| EDA end to end | All seven spec types in real and synthetic data, per-type summaries, skipped reasons, CSV inventory and HTML report sections | Every specified variable is summarised or explicitly skipped; output and report contracts agree |
| Migration | Old function aliases, old two-component EDA summaries and old CSV consumers during the compatibility window | Deprecation messages and compatibility adapters behave exactly as documented |

## Fixture Strategy

- Keep blood-storage and penguins fixtures as external regression evidence.
- Extend expected outputs only in the later implementation spec and generate them independently of package functions.
- Add a small hand-computed seven-type fixture for type dispatch and edge cases rather than expanding large fixture files for every branch.
- Preserve anti-circularity: expected statistics must come from reviewed hand calculations or an independent generator that cannot call `episcout`.

## Future Acceptance Commands

The implementation spec must define focused commands for each changed function group, then run the full suite and package check through the repository wrapper. It must also verify that tracked visual snapshots remain unchanged after test cleanup.
