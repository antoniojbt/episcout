# Future Workspace Changelog

## 2026-08-07

- Implemented issue #197/spec `019-postgresql-catalogue-missingness`: approved catalogue profiling now returns non-NULL `values` separately from one aggregate `n_missing` row per selected column, preserving empty/all-NULL evidence and keeping `max_levels` bounded to reviewable source values. Updated unit and mandatory PostgreSQL 17 tests, generated help, longitudinal guidance, the runnable database walkthrough and NEWS. Focused tests, package lint, the full suite and the local package check passed; the live test is discovered locally but awaits the repository PostgreSQL service in PR CI.
- Accepted and merged issue #198/spec `020-data-frame-writer-delimiter-contract` through PR #206 after macOS, Ubuntu, PostgreSQL, coverage, Codecov and CodeFactor checks passed; patch coverage was 100% and project coverage increased to 91.50%. Moved spec 020 to `future/specs/done/`, synchronized local/fork `master`, and activated issue #197/spec `019-postgresql-catalogue-missingness` with a pre-code contract that separates bounded non-missing value rows from one aggregate PostgreSQL NULL count per profiled column. No release-readiness audit, release, tag or deferred work was started.
- Implemented issue #198 through active spec `020-data-frame-writer-delimiter-contract`: `epi_write_df()` now maps CSV to comma bytes and TSV to tab bytes, rejects ambiguous suffixes and delimiter contradictions, preserves TSV and lower-level writer behaviour, validates the existing output directory and documents its overwrite, quoting and missing-value contract. Raw-byte/validation tests, legacy focused tests, package lint, the full suite and the local package check passed; unrelated documentation and skipped-snapshot cleanup side effects were restored. Pull-request CI, owner acceptance and merge remain open, so issue #197/spec 019 was not started.
- Created roadmap issue #204 and reconciled the planning workspace to the owner-approved sequence: issue #198/spec 020, issue #197/spec 019, release-readiness audit, release 0.3.0, the post-release PostgreSQL identifier universe as spec 021, then the narrow PostgreSQL EDA row-count fix as spec 022. Historical Codecov work and issue #196/spec 018 are explicitly deferred; no package implementation, release, tag, credential action or history rewrite was started. The release plan now treats completed spec 010 as historical evidence, the scratch index includes the identifier proposal, and the spec template uses authoritative `master` rather than the obsolete `episcout2` base.

## 2026-08-05

- Reconciled the seven open GitHub issues into explicit TODO priorities and next gates, retained only unresolved or queued scratch inputs, removed the completed spec-003 PostgreSQL backend scratch plan and recorded the noncritical PostgreSQL query-repetition finding for later disposition. No issue implementation, release or tag was started.
- Accepted and merged completed spec `003-large-data-backend-strategy` through PR #201, closing issue #194. The fixed PostgreSQL 17 synthetic gate, structural data-locality evidence, three independent reviews, local/CRAN checks and PostgreSQL/macOS/Ubuntu/coverage/CodeQL GitHub checks passed without unresolved blocker; the completed record now lives under `future/specs/done/`. No release or tag was created.
- Completed the local acceptance evidence for spec `003-large-data-backend-strategy`: the fixed PostgreSQL 17 benchmark reconciles one million rows and eight variables, publishes and validates nine SVGs, keeps non-categorical fetches within 30 rows and recorded three measured runs of 7.463, 7.216 and 7.319 seconds (median 7.319). Independent statistical, PostgreSQL/privacy and bundle/documentation reviews found no unresolved blocker after parity expectations were made complete, disposable-schema ownership was hardened, native database messages/warnings/errors were reduced to fixed value-free conditions, and SVG/manifest/categorical assertions were made explicit. Dedicated PR CI and owner acceptance were subsequently completed as recorded above.
- Amended active spec `003-large-data-backend-strategy` with owner direction to use a reproducible synthetic regression gate: one million PostgreSQL rows, eight mixed-type variables, one warm-up, three complete measured runs and a median below 120 seconds in the dedicated uninstrumented PostgreSQL CI job. The architectural data-locality evidence remains aggregate-only and bounded subject to complete categorical frequencies; the gate makes no production-runtime or comparative data-frame claim.
- Fixed longitudinal PostgreSQL pseudonymisation advisory-lock ownership for issue #195: apply now promotes protection to transaction-scoped locks, releases each acquired session lock once, retains failed releases for exit cleanup and rolls back safely if the transfer cannot complete. Live regression coverage checks warning-free success, partial-acquisition timeout cleanup and forced post-transfer rollback without changing the public result contract.
- Implemented the active PostgreSQL-backed EDA contract on `feature/postgresql-eda-backend`: added a redacted validated relation source, explicit RPostgres/PostgreSQL 17 type and transaction boundaries, aggregate-only canonical profiles and identifier QA, compact shared plot rendering, checked staged bundle publication, neutral unit/live fixtures, PostgreSQL CI coverage and user documentation. Local PostgreSQL 17 evidence covers type-7 edges, e1071 type-3 shape statistics, sentinels, infinities/NaN, Unicode text, temporal values, identifiers, relation kinds, catalogue drift, stable concurrent snapshots, lifecycle cleanup, bundle ownership/recovery refusal and SVG leakage. The acceptance evidence and independent-review results are recorded in the later entry above; no release or tag is authorised.

## 2026-08-04

- Added `future/scratch/repo-specific-spec-design-guidance-draft.md` so potentially useful specification-design practices from the hidden environment-level `ds-pipeline-designer` skill are visible and reviewable inside the repository. The draft adapts only repository-compatible contract, validation, provenance and activation-gate ideas; explicitly excludes Python defaults, foreign artifact names, scaffolding and generic architecture; and has no authority until the owner reviews and incorporates selected text into an authoritative repository file.
- Revised and activated spec `003-large-data-backend-strategy` as the PostgreSQL-first large-data EDA design. The reviewed handoff preserves data-frame dispatch and the six canonical summary components; fixes explicit relation/type, missingness, type-7/shape/Shapiro, identifier-QA, repeatable-read/read-only, compact plot-data, staged bundle, privacy and failure contracts; and maps neutral parity and bounded collection to acceptance evidence. PostgreSQL 17+ through RPostgres is the only backend in scope. The pre-code baseline is lint-clean and retains the inherited macOS DST failure, sandbox PSOCK socket restriction, one-core warning and recorded external/worktree check notes. No package code, project-specific workload material, database mutation, approximation, concurrency, release or tag is claimed by this planning entry.
- Completed spec `017-deterministic-local-time-ambiguity` for issues #190 and #81: replaced platform offset sampling with `clock` 0.7.4's bundled IANA timezone engine, blocked ambiguous, nonexistent and unsupported local times with actionable value-free guidance, preserved strict offset-bearing conversion and the public schemas, raised the R floor to 4.0, and added independently justified historical, modern, fractional, mixed-input and process-timezone tests. Focused/full tests, lint and local/CRAN checks passed; CRAN retained only recorded unrelated notes. GitHub macOS/Ubuntu verification remains for the eventual pull request because no publication was authorised.
- Activated spec `017-deterministic-local-time-ambiguity` for issues #190 and #81 to replace platform-dependent historical local-time interpretation with a bundled IANA timezone engine while preserving value-free blocking and offset-bearing datetime behaviour.
- Accepted and merged completed spec `016-longitudinal-pseudonymisation` through PR #189, then moved its planning record to `future/specs/done/`. The implementation adds generic reviewed linkage metadata, a restricted PostgreSQL identity registry, exact cross-table identity resolution, longitudinal duplicate reconciliation, atomic audit/apply output, redacted results, neutral tests, a mandatory PostgreSQL 17 integration job and a dedicated first-time-user guide. Independent integrity, leakage and documentation reviews found no unresolved blocker; 119 live expectations, package lint and GitHub PostgreSQL/package/coverage jobs passed. Codecov measured 92.05% patch and 91.21% project coverage, up 0.24 percentage points. Local/CRAN checks retained only the recorded baseline DST failure, multicore warning and inherited CRAN notes. No release or tag was created.
- Added a PostgreSQL-first design input for draft spec `003-large-data-backend-strategy`; it is now the ready-next planning task, but remains draft and does not authorise package implementation.
- Activated spec `016-longitudinal-pseudonymisation` on `feature/longitudinal-pseudonymisation`: fixed the generic reviewed linkage, restricted stable registry, exact PostgreSQL identity, longitudinal duplicate, atomic audit/apply, redaction, documentation and live-integration contracts before package changes. Recorded the clean lint baseline, 1,573 passing expectations, the existing macOS daylight-saving-time ambiguity failure and inherited local/CRAN check findings; no project-specific material, package implementation, release or tag is claimed by this planning entry.
- Separated active and completed planning material by moving completed specifications to `future/specs/done/` and completed cross-spec reviews to `future/reviews/done/`, with internal references updated to the new paths.
- Reconciled `TODOs.md` so active priorities contain no completed checkboxes, moved completed work into reverse-chronological dated entries under `Done`, and made start/end, status-change and periodic long-running-work updates explicit.

## 2026-08-03

- Completed spec `015-data-intake-to-report-workflow` for issue #184: added review-gated scaffold/audit/apply orchestration, reconciled canonical and optional stratified summaries, a fixed privacy-classified artifact manifest, transactional owned-bundle replacement and escaped aggregate-only HTML without row-level output. Independent review resolved rooted-path, regular-file, component-membership and subgroup-denominator gaps; focused tests and lint passed, and the full local check completed with 0 errors/warnings/notes. The final CRAN-oriented check retained the inherited incoming URL/vignette NOTE and added an external current-time verification NOTE; the immediately preceding run had only the inherited NOTE. No Codecov credential, tag or release was used.
- Completed spec `014-stratified-descriptive-summaries`: added deterministic grouped canonical summaries for one reviewed categorical or binary stratum and a pure, traceable long-form Table 1 renderer. Declared empty groups and levels, unexpected values, missing strata, denominators, temporal semantics and privacy-safe text aggregates remain explicit. Independent review resolved validation and presentation edge cases; focused tests passed with 93 expectations, lint was clean and the full local check completed with zero errors, warnings or notes. The CRAN-oriented check retained only the inherited external incoming URL/vignette NOTE.
- Completed spec `013-specification-guided-data-preparation`: added value-free audit and all-or-nothing apply modes, reviewed-scaffold gating, typed sentinel replacement, strict type/level/temporal preparation, explicit extra-variable policies, before/after schema reconciliation and canonical EDA integration. Independent reviews resolved modern and historical timezone ambiguity, fractional and arbitrary-offset timestamp handling, integer sentinel masking and audit completeness. Focused/full checks passed; the CRAN-oriented check retained only the external incoming URL/vignette NOTE.
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
- Completed specs were initially kept under `future/specs/`; they now live under `future/specs/done/` so active and completed work are visibly distinct.
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
    `future/reviews/done/2026-06-15-senior-r-package-review.md`.
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
