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

- Spec `020-data-frame-writer-delimiter-contract` implements issue [#198](https://github.com/antoniojbt/episcout/issues/198) on `bugfix/data-frame-writer-delimiter-contract`. Local acceptance is complete; pull-request CI, owner acceptance and merge remain open.

### Ready Next

- Issue [#197](https://github.com/antoniojbt/episcout/issues/197) follows as spec `019-postgresql-catalogue-missingness` after issue #198 is accepted and merged.

### Draft

- The multi-table PostgreSQL identifier-universe proposal remains a scratch input until its turn after release `0.3.0`, when it should become a dedicated issue and spec `021-postgresql-identity-universe`.
- The narrow redundant PostgreSQL EDA row-count finding remains a scratch input until it is promoted after the identifier-universe work as spec `022-postgresql-eda-row-count-reuse`.

### Deferred

- Historical Codecov credential containment and any conditional spec `011` work are deferred by owner direction. No history rewrite or security-policy change is authorised.
- Issue [#196](https://github.com/antoniojbt/episcout/issues/196) and spec `018-database-eda-report-rendering` are deferred until the roadmap is explicitly revised.

### Completed

Completed specs, including accepted spec `003-large-data-backend-strategy`, are under `future/specs/done/`. Each spec's `manifest.yml` remains the authoritative detailed status record.
