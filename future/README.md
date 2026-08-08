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

- No numbered implementation specification is active.

### Ready Next

- Promote issue [#215](https://github.com/antoniojbt/episcout/issues/215) to spec `021-postgresql-identity-universe`, record its baseline and implement the audit-first PostgreSQL identifier-universe contract.

### Draft

- The narrow redundant PostgreSQL EDA row-count finding remains a scratch input until it is promoted after the identifier-universe work as spec `022-postgresql-eda-row-count-reuse`.

### Deferred

- Issue [#213](https://github.com/antoniojbt/episcout/issues/213) retains only the owner-side Codecov credential and eligible cache/hidden-ref cleanup. Rewritten branches/tags and a protected-`master` upload are complete.
- CRAN readiness and submission remain deferred under issue [#81](https://github.com/antoniojbt/episcout/issues/81).
- Issue [#196](https://github.com/antoniojbt/episcout/issues/196) and spec `018-database-eda-report-rendering` are deferred until the roadmap is explicitly revised.

### Completed

GitHub release `0.3.0` is published from commit `40ef702`. Completed specs, including accepted specs `003-large-data-backend-strategy`, `019-postgresql-catalogue-missingness`, `020-data-frame-writer-delimiter-contract`, `023-package-source-hygiene` and `024-external-fixture-provenance`, are under `future/specs/done/`. Each spec's `manifest.yml` remains the authoritative detailed status record.
