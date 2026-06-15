# Future SDD/TDD Workspace

This directory is the active planning workspace for next-phase `episcout`
development. It is committed to Git but excluded from R package builds by
`.Rbuildignore`.

Use this workspace for internal design notes, TDD plans, review prompts and
agent handoff material. Keep executable package tests in `tests/testthat/`.

## Directory Structure

```text
future/
├── README.md
├── backlog.md
├── decisions.md
├── changelog.md
├── prompts/
├── references/
├── reviews/
├── specs/
└── scratch/
```

## Workflow

1. Add candidate work to `backlog.md`.
2. Promote active work to a numbered directory under `specs/`.
3. Fill in `brief.md`, `sdd.md`, `tdd.md`, `acceptance.md` and
   `manifest.yml` before implementation.
4. Implement executable tests under `tests/testthat/`.
5. Implement package code under `R/`, `inst/` or other package directories.
6. Record review notes in the spec `review.md`.
7. Update acceptance status and `changelog.md`.

## R Command Policy

Use the repo-local wrapper for R commands:

```bash
scripts/rscript_env_caller.R -e "devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::check(manual = FALSE)"
```

Do not use bare `Rscript` in future specs or check instructions.

## Active Specs

- `001-phase-1-helper-stabilization`
- `002-penguins-raw-fixture`
- `003-large-data-backend-strategy`
