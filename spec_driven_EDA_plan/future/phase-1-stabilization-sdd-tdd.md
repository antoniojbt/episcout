# Deferred Phase 1 stabilization plan

## Status

Phase 1 in `spec_driven_EDA_plan/docs/roadmap.md` is not complete.

`spec_driven_EDA_plan/docs/repository-audit.md` partially satisfies the audit
portion by identifying fragile existing helpers, but there is no completed
Phase 1 PR sequence, baseline check record, coverage baseline, or targeted
hardening PR.

This work is intentionally deferred. It must not block the active PR-sized task
in `spec_driven_EDA_plan/docs/START_HERE.md`.

Fragile was identified as edge cases which can produce warnings, unclear errors, or unstable numeric results. The functions are likely fine for common data, but their contracts are under-specified at the boundaries. TDD hardening would make those boundaries explicit.

For instance:

For `epi_stats_numeric()`:

- **All-missing vectors:** calls like `min(x, na.rm = TRUE)`, `max()`, `quantile()`, `IQR()`, `sd()`, `var()`, `skewness()`, and `kurtosis()` do not all have clean, consistent behavior when there are no usable values.
- **Zero-length vectors:** `n = 0` makes `NA_percentage <- (NA_count / n) * 100` become division by zero/`NaN`.
- **Mean is zero:** `CV = sd / mean` can become `Inf`, `-Inf`, or `NaN`. That needs an explicit contract.
- **Shapiro-Wilk handling:** the code checks `n_nonNA`, but passes the original vector to `shapiro.test()`. If there are `NA`s, it relies on `tryCatch()` rather than intentionally passing cleaned values.
- **Repeated calculations:** `sd()` and `mean()` are recalculated multiple times, increasing room for inconsistent handling when edge cases are added.
- **Dependency behavior:** it hard-stops if `e1071` is unavailable, even though some simpler stats could still be computed. Since `e1071` is in `Imports`, that is acceptable in installed-package use, but it makes local/source testing brittle if dependencies are not installed.

For `epi_stats_na_perc()`:

- **Uses `apply()` on data frames:** `apply()` coerces data frames to a matrix first. With mixed types, that can silently coerce numbers, characters, dates, factors, etc. before counting.
- **Invalid `margin`:** if `margin` is not `1` or `2`, the function falls through and returns an undefined object error: `object 'na_perc_all' not found`.
- **Return shape differs by mode:** column mode returns transposed row names as variable labels; row mode returns a plain data frame from an unnamed vector. That can be okay, but it needs a stable tested contract.
- **No input validation:** `df = NULL`, non-data-frame input, zero-row/zero-column inputs, and invalid margins are not handled deliberately.

For `epi_stats_summary()`:

- **Older tidyverse idioms:** `select_if()` still works in many setups, but it is superseded and more likely to become noisy or awkward in future dplyr versions.
- **`expression()` + `eval()` dispatch:** harder to reason about, harder to test, and easier to break when refactoring.
- **Depends on lower helpers:** if `epi_stats_numeric()` is fragile, then numeric summary mode inherits that fragility.


## Decision

Do not interrupt the current specification-first TDD sequence to complete Phase
1. Continue the active PR flow unless the user explicitly asks to activate this
deferred stabilization plan.

When activated, Phase 1 stabilization should be a small, TDD-first hardening PR
for existing helper behaviour, not a broad modernization of the package.

## Why this is deferred

The current spec-first EDA work is already organized as small TDD slices:

```text
external fixture and expected output first
failing fixture-backed tests second
minimal implementation third
```

The active spec-first tasks do not yet require broad changes to older `epi_*`
helpers. Completing Phase 1 now would add scope and review risk without being
necessary for the current PR.

The older helpers still matter, but they should be hardened when new work
depends on them or when package checks expose concrete failures.

## Reopen triggers

Reopen this deferred Phase 1 work when any of the following is true:

- `epi_eda_profile_summaries()` depends on `epi_stats_numeric()` or
  `epi_stats_summary()`.
- `epi_eda_run()` depends on older `epi_*` helpers.
- CI, `R CMD check`, or local tests expose helper failures.
- The package is being prepared for release or broader hardening.
- The user explicitly asks to run the deferred Phase 1 stabilization PR.

## SDD intent

The intent is to preserve existing public behaviour while making fragile helper
edge cases explicit and testable.

Phase 1 stabilization should:

- keep existing exported function names and return shapes stable where
  practical;
- improve clear failure modes for invalid inputs;
- avoid hidden state and broad refactoring;
- document any intentional behaviour changes in tests and roxygen comments;
- leave the specification-first API sequence intact unless a direct dependency
  requires coordination.

## TDD sequence

Use this sequence when the deferred work is activated:

1. Record the local baseline status for tests/checks before changing behaviour.
2. Add failing tests for the selected helper edge cases.
3. Implement the smallest code change that passes those tests.
4. Run focused tests for the changed helpers.
5. Run broader package tests/checks where practical.
6. Fix only failures caused by this PR.
7. Document any pre-existing or unrelated failures in the PR summary.

Do not write tests that merely lock in current broken behaviour. Tests should
describe the intended stable contract.

## Initial edge cases

Start with these edge cases:

- `epi_stats_numeric()` with all-missing numeric vectors.
- `epi_stats_numeric()` with zero-length vectors.
- `epi_stats_numeric()` coefficient of variation when mean is zero.
- `epi_stats_numeric()` normality, skewness, and kurtosis behaviour with
  insufficient usable values.
- `epi_stats_na_perc()` with invalid `margin`.
- `epi_stats_na_perc()` with mixed-type data frames.
- `epi_stats_na_perc()` return shape for row and column modes.

Prefer one small PR for `epi_stats_numeric()` and `epi_stats_na_perc()` before
expanding to other helpers.

## Acceptance criteria

The deferred Phase 1 PR is complete when:

- failing tests are added before implementation changes;
- `epi_stats_numeric()` and `epi_stats_na_perc()` handle the selected edge cases
  deterministically;
- public function names remain unchanged;
- return objects remain data frames or tibbles compatible with existing tests;
- invalid input errors are clear and actionable;
- baseline and final local check/test status are recorded in the PR summary;
- unrelated helper modernization is not included.

## Out of scope

Do not include these unless a failing test or check makes them necessary:

- broad replacement of all `select_if()` calls;
- redesign of the public `epi_*` API;
- removal or renaming of exported functions;
- changes to fixture files;
- changes to spec-first implementation files;
- report or project-template work;
- large-data backend implementation;
- package-wide style rewrites.
