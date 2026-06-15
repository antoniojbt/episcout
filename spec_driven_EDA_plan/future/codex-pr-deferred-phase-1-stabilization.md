# Codex PR instruction: deferred Phase 1 stabilization

## Intended use

Pass this file directly to Codex only when you want to activate the deferred
Phase 1 stabilization work.

This instruction is not part of the live PR sequence. Do not run it unless the
user explicitly asks to do deferred Phase 1 stabilization.

## Repository context

Repository:

```text
antoniojbt/episcout
```

Target branch:

```text
episcout2
```

Current live workflow:

```text
spec_driven_EDA_plan/docs/START_HERE.md
```

The live workflow remains the source of truth for the active spec-first PR
sequence. This deferred Phase 1 PR should not change the active PR unless the
user explicitly requests that.

## When to use this

Use this instruction when one of these triggers is present:

- `epi_eda_profile_summaries()` depends on `epi_stats_numeric()` or
  `epi_stats_summary()`.
- `epi_eda_run()` depends on older `epi_*` helpers.
- CI, `R CMD check`, or local tests expose helper failures.
- A release or broader package hardening pass is being prepared.
- The user explicitly asks to activate deferred Phase 1 stabilization.

If none of these triggers is present, continue the active PR-sized task in
`START_HERE.md` instead.

## Required reading

Read these files before making changes:

```text
AGENTS.MD
spec_driven_EDA_plan/docs/START_HERE.md
spec_driven_EDA_plan/docs/roadmap.md
spec_driven_EDA_plan/docs/repository-audit.md
spec_driven_EDA_plan/future/phase-1-stabilization-sdd-tdd.md
spec_driven_EDA_plan/docs/codex/review-checklist.md
```

## PR goal

Create a small TDD-first hardening PR for the highest-risk existing helper edge
cases without changing the current spec-first EDA PR sequence.

The first deferred Phase 1 PR should focus on:

```text
epi_stats_numeric()
epi_stats_na_perc()
baseline local check/test status
```

Do not broaden the PR into package-wide modernization.

## Scope

This PR should:

- record baseline local test/check status before behaviour changes;
- add failing tests for selected helper edge cases;
- implement only the helper changes needed to pass those tests;
- preserve backwards compatibility where practical;
- document pre-existing or unrelated failures rather than fixing them.

## Must edit

Edit only the files needed for the selected helper tests and implementation.

Expected files for the first deferred Phase 1 PR:

```text
tests/testthat/test-epi_stats_numeric.R
tests/testthat/test-stats.R
R/epi_stats_numeric.R
R/epi_stats_na_perc.R
```

If a narrower test file is clearer, create or edit a focused test file under:

```text
tests/testthat/
```

## May edit

Only if required by roxygen/documentation updates:

```text
man/
NAMESPACE
```

Only if the PR needs to record the deferred Phase 1 closeout:

```text
spec_driven_EDA_plan/future/phase-1-stabilization-sdd-tdd.md
```

## Must not edit

Do not edit current spec-first implementation files unless a failing test proves
that the deferred hardening PR directly requires it:

```text
R/epi_eda_spec.R
R/eda_schema.R
R/eda_missing.R
R/eda_synthetic.R
R/eda_summaries.R
R/eda_plots.R
R/epi_eda_run.R
R/eda_report.R
```

Do not edit fixture files:

```text
tests/testthat/fixtures/
```

Do not edit report or project templates:

```text
inst/report-template/
inst/project-template/
```

Do not edit active PR task files from:

```text
spec_driven_EDA_plan/docs/START_HERE.md
```

unless the user explicitly asks to change the live workflow.

## TDD requirements

Add tests before implementation for the selected edge cases.

Initial test targets:

- `epi_stats_numeric()` handles all-missing numeric vectors.
- `epi_stats_numeric()` handles zero-length numeric vectors.
- `epi_stats_numeric()` returns a stable coefficient of variation when mean is
  zero.
- `epi_stats_numeric()` skips normality, skewness, and kurtosis safely when
  there are insufficient usable values.
- `epi_stats_na_perc()` rejects invalid `margin` with a clear error.
- `epi_stats_na_perc()` handles mixed-type data frames without coercion-related
  count errors.
- `epi_stats_na_perc()` keeps a stable return shape for row and column modes.

Tests should define the desired contract explicitly. Do not write tests that
only preserve accidental current behaviour.

## Baseline commands

Run these before implementation where practical:

```bash
Rscript -e "devtools::test(reporter = 'summary')"
Rscript -e "devtools::check(manual = FALSE)"
Rscript -e "covr::package_coverage()"
```

If required packages are missing locally, do not install them unless the user
approves. Record the missing dependency and continue with focused tests that can
run in the available environment.

Run focused tests after adding failing tests and after implementation. Examples:

```bash
Rscript -e "testthat::test_file('tests/testthat/test-epi_stats_numeric.R')"
Rscript -e "testthat::test_file('tests/testthat/test-stats.R')"
```

Run broader checks after implementation where practical:

```bash
Rscript -e "devtools::document()"
Rscript -e "devtools::test(reporter = 'summary')"
Rscript -e "devtools::check(manual = FALSE)"
```

## Implementation constraints

Keep the change small and reviewable.

Preserve:

- exported function names;
- argument names where practical;
- data-frame-like return values;
- existing successful tests;
- backwards compatibility unless an old behaviour is clearly erroneous and now
  covered by tests.

Avoid:

- package-wide refactoring;
- broad `select_if()` modernization;
- adding heavy dependencies;
- changing the spec-first EDA API;
- changing fixtures or expected fixture outputs;
- fixing unrelated audit findings in the same PR.

## Acceptance criteria

The PR is ready when:

- selected edge-case tests fail before implementation and pass after;
- focused helper tests pass;
- broader package tests/checks are run where practical;
- any missing local dependencies or pre-existing failures are recorded;
- no files outside the deferred Phase 1 scope are changed;
- the current `START_HERE.md` active PR is not changed unless explicitly
  requested.

## Closeout

Summarise:

- why deferred Phase 1 was activated;
- files changed;
- tests added;
- helper behaviours hardened;
- checks run and results;
- expected or pre-existing failures;
- any follow-up Phase 1 work left out of scope.

Do not mark Phase 1 as globally complete unless all roadmap Phase 1 goals are
actually satisfied:

```text
run R CMD check
run test suite
record baseline coverage
identify fragile functions
fix clear edge cases
```
