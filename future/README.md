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

GitHub issues and the current roadmap issue are the live source of truth. `TODOs.md`, specification manifests and `changelog.md` are synchronised repository records; do not start later work while they disagree with GitHub.

1. Record candidate work in a GitHub issue and add it to the current roadmap or explicitly mark it deferred. Mirror the actionable queue in `TODOs.md`.
2. Promote consequential or multi-step work to a numbered directory under `specs/`, set its versioned manifest to `draft` and complete `brief.md`, `sdd.md`, `tdd.md`, `acceptance.md` and `review.md` before implementation.
3. Resolve every activation gate, set the manifest to `active` and record the baseline before changing package code. Keep at most one implementation spec `active` or `review`.
4. Write executable tests under `tests/testthat/`, implement package code under current package directories, update user documentation and generated files together, and record review evidence.
5. Set the manifest to `review` when its pull request opens. The PR may close the tracking issue only when it delivers the issue's complete outcome; planning-only work must first create its successor implementation issue or record a terminal reason.
6. Treat a merged PR whose manifest or trackers still say `review`, `active` or `implemented` as `merged/pending-closeout`. Do not begin the next task.
7. During closeout, verify the canonical default-branch merge and required checks, reconcile the tracking issue and roadmap, finalize acceptance and review evidence, set the manifest to `completed`, move the spec to `specs/done/`, update `TODOs.md` and `changelog.md`, and confirm the successor issue or terminal reason.
8. Use a focused closeout PR when merge-derived evidence must change tracked files. Closeout-only PRs do not require a recursive documentation PR.
9. Run `scripts/check-workflow-state.sh` at task start, review handoff and closeout. Move completed cross-spec reviews to `reviews/done/`.

### Manifest lifecycle fields

New and current manifests use `workflow_version: 1`, `deliverable`, `status`, `tracking_issue`, `source_issues`, `pull_request`, `merge_commit`, `successor_issue` and `terminal_reason`. Valid committed statuses are `draft`, `active`, `review` and `completed`. Historical completed manifests without `workflow_version` remain valid legacy records.

## R Command Policy

Use the repo-local wrapper for R commands:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```

Do not use bare `Rscript` in future specs or check instructions. For `devtools::check()`, always set an explicit CRAN mirror instead of relying on `@CRAN@`; otherwise checks can spend a long time probing repository indexes.

## Current Status

Roadmap issue [#227](https://github.com/antoniojbt/episcout/issues/227) is the authoritative return point. `TODOs.md` mirrors its ready-next, parallel and deferred work without replacing live GitHub state.

Completed spec `027-epi-geo-phase-a` records issue [#226](https://github.com/antoniojbt/episcout/issues/226) and PR [#234](https://github.com/antoniojbt/episcout/pull/234), merged to canonical `master` as `b37b391`. Completed spec `028-epi-geo-postgis` records issue [#233](https://github.com/antoniojbt/episcout/issues/233) and PR [#238](https://github.com/antoniojbt/episcout/pull/238), merged to canonical `master` as `460acd0`. Completed terminal Phase-C spec `029-eda-reviewed-coordinate-roles` records issue [#237](https://github.com/antoniojbt/episcout/issues/237), planning PR [#240](https://github.com/antoniojbt/episcout/pull/240) and implementation PR [#241](https://github.com/antoniojbt/episcout/pull/241), merged as `308d544`; all required CI passed.

Issue [#243](https://github.com/antoniojbt/episcout/issues/243) is active as spec `030-simplify-core-eda-controls-geo-outputs`. Planning/prerequisite PR #244 is fully green at `8c4935f`; implementation is continuing on a stacked draft branch under the owner's explicit instruction, with #244 required to merge first.

Completed specifications live under `specs/done/`. Release `0.3.0` remains published from commit `40ef702`; CRAN work remains deferred under issue #81, and owner-only Codecov containment remains parallel under issue #213.
