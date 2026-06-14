# Deferred Phase 1 stabilization plan

## Status

Phase 1 in `spec_driven_EDA_plan/docs/roadmap.md` is not complete.

`spec_driven_EDA_plan/docs/repository-audit.md` partially satisfies the audit
portion by identifying fragile existing helpers, but there is no completed
Phase 1 PR sequence, baseline check record, coverage baseline, or targeted
hardening PR.

This work is intentionally deferred. It must not block the active PR-sized task
in `spec_driven_EDA_plan/docs/START_HERE.md`.

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

- `profile_summaries()` depends on `epi_stats_numeric()` or
  `epi_stats_summary()`.
- `run_eda()` depends on older `epi_*` helpers.
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
