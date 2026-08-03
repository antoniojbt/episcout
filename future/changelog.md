# Future Workspace Changelog

## 2026-08-03

- Completed spec `012-data-frame-eda-spec-scaffold`: added the review-required `epi_eda_spec_scaffold()` contract, conservative storage and candidate classification, privacy-safe aggregate evidence, all-or-nothing structural validation, exact scaffold CSV round-tripping, independently authored edge tests, a data-first vignette workflow and generated documentation. Focused tests and `scripts/check-local.sh` passed; `scripts/check-cran.sh` completed with only the external CRAN-index/URL incoming NOTE.
- Completed spec `010-canonical-eda-summary-contract` after PRs #178 and #179 merged. Repeated `scripts/check-local.sh` from the combined default-branch tip with zero errors, warnings or notes, retained the two known environment skips, restored check-generated documentation and snapshot drift, and created no tag or release.

## 2026-07-25

- Completed spec `009-repository-lint-style-cleanup`: reduced the package-loaded lint baseline from 163 findings to zero through a targeted 33-file cleanup, preserved the `%>%` compatibility policy and historical public contracts with narrow documented exceptions, removed dynamic `eval()` from current-mode `epi_stats_summary()`, added failing local and CI lint gates, aligned contributor guidance, passed focused and full tests with the two known skips, and completed package check with only the accepted `.gitkeep` NOTE.
- Completed spec `008-univariate-stats-eda-alignment`: introduced shared univariate summary cores, preserved EDA v1 as the compatibility default, added complete opt-in v2 summaries and CSV/report outputs for all seven specification types, added typed `epi_stats_summary()` output, aligned scoped active public wrappers, corrected confirmed edge defects, extended blood-storage and penguins regression evidence, and passed the full suite and package check without new warnings or notes.
- Triaged `future/ideas/`: removed the superseded pseudonymisation proposal and manual Codex environment script, archived resolved cross-spec PR review notes under `future/reviews/`, and promoted the EDA/statistics alignment proposal into completed design-only spec `007-eda-stats-alignment-review`.
- Completed spec `007-eda-stats-alignment-review`: confirmed `epi_stats_*` as the active main statistics layer, inventoried all 25 exported `epi_stats_*` functions plus the related non-prefixed contingency helper, mapped all seven EDA specification types, recorded confirmed behavioural gaps, and recommended a shared-core architecture and ordered implementation programme without changing package code.
- Reconciled spec status after the 0.2.0 release. Specs 001, 002, 004, 005 and
  006 are completed; spec 003 remains draft.
- Completed spec `002-penguins-raw-fixture`.
  - Added the pinned 344-row, 17-column `palmerpenguins::penguins_raw` fixture
    with provenance and a reviewed data dictionary.
  - Added independent schema, missingness, numeric summary, categorical summary
    and non-visual plot-dispatch expectations.
  - Added offline executable consumers for every expected output and a source
    guard against calls to the package under test.
  - Preserved all 336 upstream missing cells through CSV serialization.
  - Extended the existing generator to rebuild both external fixtures and
    updated regeneration commands to use the repo-local R wrapper.
  - Targeted tests and the full package suite passed; `devtools::check()` passed
    with the existing `.gitkeep` NOTE.
- Kept completed specs under `future/specs/` so existing references remain stable instead of introducing a separate `future/done/` tree.
- Triaged deferred PR review notes and completed spec `006-synthetic-integer-generation`.
  - Corrected singleton integer sampling and rejected bounds containing no
    integer values.
  - Added non-vacuous range/level assertions and focused integer-domain tests.
  - Targeted synthetic tests and the full package test suite passed.
  - `devtools::check(manual = FALSE)` passed with the existing `.gitkeep` NOTE.
  - Restored tracked `vdiffr` snapshots removed by the known skipped-snapshot
    cleanup behavior.
- Revised spec 002 to extend the existing external-fixture generator and to require executable consumers for all committed expected outputs.
- Deferred spec 003 until a concrete workload and measurable performance target are available.

## 2026-06-15

- Created root-level `future/` SDD/TDD workspace.
- Migrated deferred Phase 1 stabilization into `specs/001-phase-1-helper-stabilization/`.
- Migrated the proposed `penguins_raw` fixture PR into `specs/002-penguins-raw-fixture/`.
- Added large-data backend design follow-up under `specs/003-large-data-backend-strategy/`.
- Added templates, prompts, todos, references and review folders.
- Implemented `specs/001-phase-1-helper-stabilization/` with TDD coverage for numeric summary and missingness helper edge cases.
- Implemented `specs/004-senior-review-followups/` with EDA `missing_codes`, non-circular summary tests, stable all-missing summaries, categorical `p_observed`, and parallel dependency guards.
- Standardised future R check commands to set an explicit CRAN mirror (`https://cloud.r-project.org`) before `devtools` checks.
- Completed a thorough code review using `future/prompts/senior-r-package-review.md`.
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
- Reviewed future work setup for SDD/TDD, including PR14 and `penguins_raw` follow-ups.
