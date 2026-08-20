# Future SDD/TDD Workspace

This directory is the active planning workspace for next-phase `episcout` development. It is committed to Git but excluded from R package builds by `.Rbuildignore`.

Use this workspace for internal design notes, TDD plans, review prompts and agent handoff material. Keep executable package tests in `tests/testthat/`.

See `../PROJECT_MAP.md` for the current package and repository map.

## Directory Structure

```text
future/
├── README.md
├── TODOs.md
├── decisions.md
├── changelog.md
├── prompts/
├── references/
├── reviews/
│   └── done/
├── specs/
│   └── done/
└── scratch/
```

## Workflow

GitHub issues and pull requests are the live work record. Use a numbered specification when consequential semantics, several dependent components, migration risk or cross-session hand-off make a written contract useful. Small and well-defined work does not require a specification or roadmap.

1. Define the outcome, interfaces, important decisions and verification in the issue, specification or task record appropriate to the work.
2. When a numbered specification is useful, keep its brief, design, test and acceptance records together under `specs/` and update its manifest as the work progresses.
3. Write executable tests under `tests/testthat/`, implement package code under current package directories, and update affected user documentation and generated files together.
4. Run `scripts/check-workflow-state.sh` when the task uses a numbered specification. The script checks that workflow's records; it is not a prerequisite for work outside it.
5. After merge, update only records whose current claims changed. Move a completed specification to `specs/done/` when doing so makes the active planning area clearer.

### Manifest lifecycle fields

Existing manifests use `workflow_version: 1`, `deliverable`, `status`, `tracking_issue`, `source_issues`, `pull_request`, `merge_commit`, `successor_issue` and `terminal_reason`. The current checker recognises `draft`, `active`, `review` and `completed`. These fields describe specifications that use this optional workflow; they are not repository-wide work states. Historical completed manifests without `workflow_version` remain retained records.

## R Command Policy

Use the repo-local wrapper for R commands:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```

Do not use bare `Rscript` in future specs or check instructions. For `devtools::check()`, always set an explicit CRAN mirror instead of relying on `@CRAN@`; otherwise checks can spend a long time probing repository indexes.

## Current Status

Issue [#358](https://github.com/antoniojbt/episcout/issues/358)/spec 054 completed through PR #367, merged to canonical `master` as `3d8bbdb`. The design-only audit reuses stratified EDA for cross-tabs, retains project plotting and SIAP semantics downstream, and created separately blocked successor [#369](https://github.com/antoniojbt/episcout/issues/369) for explicit-pair Spearman, aggregate-count Cramér's V and bounded stratified domains. The completed record is under `future/specs/done/`; no implementation is active.

Probabilistic-linkage issues [#360](https://github.com/antoniojbt/episcout/issues/360), [#361](https://github.com/antoniojbt/episcout/issues/361) and [#362](https://github.com/antoniojbt/episcout/issues/362) completed through PRs #363, #364 and #365. Specs 052 and 053 are archived under `future/specs/done/`; no automatic successor is authorised and no roadmap is active.

Completed spec `027-epi-geo-phase-a` records issue [#226](https://github.com/antoniojbt/episcout/issues/226) and PR [#234](https://github.com/antoniojbt/episcout/pull/234), merged to canonical `master` as `b37b391`. Completed spec `028-epi-geo-postgis` records issue [#233](https://github.com/antoniojbt/episcout/issues/233) and PR [#238](https://github.com/antoniojbt/episcout/pull/238), merged to canonical `master` as `460acd0`. Completed terminal Phase-C spec `029-eda-reviewed-coordinate-roles` records issue [#237](https://github.com/antoniojbt/episcout/issues/237), planning PR [#240](https://github.com/antoniojbt/episcout/pull/240) and implementation PR [#241](https://github.com/antoniojbt/episcout/pull/241), merged as `308d544`; all required CI passed.

Completed spec `030-simplify-core-eda-controls-geo-outputs` records issue [#243](https://github.com/antoniojbt/episcout/issues/243), planning PR [#244](https://github.com/antoniojbt/episcout/pull/244) and implementation PR [#246](https://github.com/antoniojbt/episcout/pull/246), merged to canonical `master` as `825215e`. Completed specs `031-canonical-eda-delivery`, `032-eda-denominator-gap-assessment` and `033-categorical-denominator-presentation` record the ordered #245/#248/#253 delivery and analytical-clarity sequence, ending with planning PR [#257](https://github.com/antoniojbt/episcout/pull/257) and implementation PR [#258](https://github.com/antoniojbt/episcout/pull/258), merged as `49bf7c4` and `074f13a`; all required CI passed.

Completed design [issue-276](https://github.com/antoniojbt/episcout/issues/276)/`spec-034-retained-epi-sec-technical-contract` records the accepted technical contract under owner roadmaps [issue-274](https://github.com/antoniojbt/episcout/issues/274) and [issue-275](https://github.com/antoniojbt/episcout/issues/275). Design [PR-277](https://github.com/antoniojbt/episcout/pull/277) merged as `commit-8641abe`, all required CI passed, and closeout became canonical at `commit-cc05cb0`. Implementations [issue-278](https://github.com/antoniojbt/episcout/issues/278)/`spec-035-identity-universe-technical-contract`, [issue-284](https://github.com/antoniojbt/episcout/issues/284)/`spec-036-epi-sec-registry-neutral` and [issue-285](https://github.com/antoniojbt/episcout/issues/285)/`spec-037-epi-sec-linkage-results-neutral` completed through [PR-281](https://github.com/antoniojbt/episcout/pull/281), final [PR-289](https://github.com/antoniojbt/episcout/pull/289) and [PR-291](https://github.com/antoniojbt/episcout/pull/291), merged as `commit-ebd8d35`, `commit-40b284f` and `commit-0d9b302`. Documentation predecessor [issue-268](https://github.com/antoniojbt/episcout/issues/268)/[PR-295](https://github.com/antoniojbt/episcout/pull/295) completed at `commit-d79dd3c`; terminal [issue-269](https://github.com/antoniojbt/episcout/issues/269)/`spec-038-longitudinal-pseudonymisation-documentation` completed through [PR-296](https://github.com/antoniojbt/episcout/pull/296) at `commit-e135251` with no package-code successor.

Completed [issue-271](https://github.com/antoniojbt/episcout/issues/271)/`spec-039-reviewable-qc-cleaning-proposals`, [issue-272](https://github.com/antoniojbt/episcout/issues/272)/`spec-040-approved-cleaning-rules-and-processed-outputs` and terminal [issue-273](https://github.com/antoniojbt/episcout/issues/273)/`spec-041-reviewed-civil-date-derivation` record implementations [PR-299](https://github.com/antoniojbt/episcout/pull/299), [PR-301](https://github.com/antoniojbt/episcout/pull/301) and [PR-303](https://github.com/antoniojbt/episcout/pull/303), merged as `commit-78a4776`, `commit-6713a79` and `commit-db839d0`. This cleaning lane has no automatic successor.

Completed specifications live under `specs/done/`. Releases `0.4.0` and `0.4.1` are published from commits `ea2a3f1` and `caa2812`; the immutable 0.4.1 tag retained `Version: 0.4.0` in `DESCRIPTION`. CRAN work remains deferred under issue #81, and owner-only Codecov containment remains parallel under issue #213.
