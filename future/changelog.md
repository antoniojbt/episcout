# Future Workspace Changelog

## 2026-06-15

- Created root-level `future/` SDD/TDD workspace.
- Migrated deferred Phase 1 stabilization into
  `specs/001-phase-1-helper-stabilization/`.
- Migrated the proposed `penguins_raw` fixture PR into
  `specs/002-penguins-raw-fixture/`.
- Added large-data backend design follow-up under
  `specs/003-large-data-backend-strategy/`.
- Added templates, prompts, todos, references and review folders.
- Implemented `specs/001-phase-1-helper-stabilization/` with TDD coverage for
  numeric summary and missingness helper edge cases.
- Implemented `specs/004-senior-review-followups/` with EDA `missing_codes`,
  non-circular summary tests, stable all-missing summaries, categorical
  `p_observed`, and parallel dependency guards.
- Standardised future R check commands to set an explicit CRAN mirror
  (`https://cloud.r-project.org`) before `devtools` checks.
- Completed a thorough code review using
  `future/prompts/senior-r-package-review.md`.
  - Wrote the review to
    `future/reviews/2026-06-15-senior-r-package-review.md`.
  - Corrected the confirmed `epi_stats_numeric()` kurtosis documentation issue.
  - Planned follow-up work as `004-senior-review-followups` candidate.
- Completed spec `004-senior-review-followups`.
  - Implemented EDA `missing_codes` handling in missingness and summaries.
  - Replaced circular summary expectations with hand-computed tests.
  - Added stable all-missing numeric summary behavior and categorical
    `p_observed`.
  - Deferred tidy-eval public API cleanup.
  - Ran final `devtools::test(reporter = 'summary')`, which passed.
  - `devtools::check(manual = FALSE)` passed with one existing `.gitkeep` NOTE.
- Completed spec `001-phase-1-helper-stabilization`.
  - Recorded the baseline before package-code changes.
  - Added TDD edge-case tests before implementation.
  - Scoped implementation to `epi_stats_numeric()` and `epi_stats_na_perc()`.
  - Ran final `devtools::test(reporter = 'summary')`, which passed.
- Reviewed future work setup for SDD/TDD, including PR14 and `penguins_raw`
  follow-ups.
