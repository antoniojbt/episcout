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

1. Add candidate work to `TODOs.md`.
2. Promote active work to a numbered directory under `specs/`; only draft and active specs remain there.
3. Fill in `brief.md`, `sdd.md`, `tdd.md`, `acceptance.md` and `manifest.yml` before implementation.
4. Implement executable tests under `tests/testthat/`.
5. Implement package code under `R/`, `inst/` or other package directories.
6. Record review notes in the spec `review.md`.
7. Reconcile `TODOs.md`, acceptance status and `changelog.md` whenever work changes status; also check them at the start and end of each `future/` change and periodically during long-running work.
8. Move an accepted completed spec to `specs/done/`, and move a completed cross-spec review to `reviews/done/`.

## R Command Policy

Use the repo-local wrapper for R commands:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```

Do not use bare `Rscript` in future specs or check instructions. For `devtools::check()`, always set an explicit CRAN mirror instead of relying on `@CRAN@`; otherwise checks can spend a long time probing repository indexes.

## Spec Status

There are no untriaged candidates under `future/ideas/`. Add new candidate work to `TODOs.md` before promoting it to a numbered specification.

### Active

- Implemented specs `023-package-source-hygiene` and `024-external-fixture-provenance` consolidate issues [#208](https://github.com/antoniojbt/episcout/issues/208) and [#209](https://github.com/antoniojbt/episcout/issues/209) on `refactor/release-unblockers-208-209`. They remove exact audited archive artifacts and add fail-closed fixture provenance without changing package behaviour.

### Ready Next

- After the combined #208/#209 PR merges, finish the owner-only replacement of the five rewritten upstream release tags and revoke/rotate the historical Codecov credential without recording either value.
- Release issue [#81](https://github.com/antoniojbt/episcout/issues/81) is then ready to prepare a usable GitHub `0.3.0`; CRAN submission polish is explicitly deferred.

### Draft

- The multi-table PostgreSQL identifier-universe proposal remains a scratch input until its turn after release `0.3.0`, when it should become a dedicated issue and spec `021-postgresql-identity-universe`.
- The narrow redundant PostgreSQL EDA row-count finding remains a scratch input until it is promoted after the identifier-universe work as spec `022-postgresql-eda-row-count-reuse`.

### Deferred

- The owner authorised the Codecov history rewrite on 2026-08-07. Rewritten `master` and fork heads are published; five upstream annotated tags, credential revocation/rotation and protected-branch upload verification remain owner-only follow-up.
- Issue [#196](https://github.com/antoniojbt/episcout/issues/196) and spec `018-database-eda-report-rendering` are deferred until the roadmap is explicitly revised.

### Completed

Completed specs, including accepted specs `003-large-data-backend-strategy`, `019-postgresql-catalogue-missingness` and `020-data-frame-writer-delimiter-contract`, are under `future/specs/done/`. Each spec's `manifest.yml` remains the authoritative detailed status record.
