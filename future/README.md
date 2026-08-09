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

- Issue [#220](https://github.com/antoniojbt/episcout/issues/220) is active as spec `022-postgresql-eda-row-count-reuse`, a bounded internal correction that reuses the transaction-local PostgreSQL EDA row count.

### Ready Next

- Review the draft contract for issue [#217](https://github.com/antoniojbt/episcout/issues/217) after spec 022 is handed off; package-code activation remains gated on accepting the authoritative CURP design.

### Draft

- Issue [#217](https://github.com/antoniojbt/episcout/issues/217) is the next planning candidate after spec 022. It needs an authoritative CURP validation, derivation, quality and privacy contract before becoming spec `025-curp-validation-and-reconciliation`; the issue's referenced photo is not present in the repository or its Git history.

### Deferred

- Issue [#213](https://github.com/antoniojbt/episcout/issues/213) retains only the owner-side Codecov credential and eligible cache/hidden-ref cleanup. Rewritten branches/tags and a protected-`master` upload are complete.
- CRAN readiness and submission remain deferred under issue [#81](https://github.com/antoniojbt/episcout/issues/81).
- Issue [#196](https://github.com/antoniojbt/episcout/issues/196) and spec `018-database-eda-report-rendering` are deferred until the roadmap is explicitly revised.

### Completed

GitHub release `0.3.0` is published from commit `40ef702`. Completed specs, including accepted specs `003-large-data-backend-strategy`, `019-postgresql-catalogue-missingness`, `020-data-frame-writer-delimiter-contract`, `021-postgresql-identity-universe`, `023-package-source-hygiene` and `024-external-fixture-provenance`, are under `future/specs/done/`. Each spec's `manifest.yml` remains the authoritative detailed status record.
