# Archived AGENTS.md

This file is historical guidance for the archived specification-first EDA
planning flow. Use root `AGENTS.MD` and `future/README.md` for new work.

## Scope

This repository is an R package. The specification-first EDA plan in this
directory is archived reference material.

## Required reading

For historical context, read:

- archive/eda_sdd_tdd_r1_archive/START_HERE.md
- archive/eda_sdd_tdd_r1_archive/codex/tdd-first-codex-instructions.md
- archive/eda_sdd_tdd_r1_archive/codex/review-checklist.md

Do not treat archived `START_HERE.md` as the live source of truth.

## Working rules

- Use the root `future/` workspace for active PR-sized tasks.
- Do not broaden scope.
- Do not perform unrelated refactoring.
- Respect must-edit and must-not-edit lists.
- Do not use episcout functions to generate expected fixture outputs.
- Use external fixtures and independently computed expected outputs.
- Do not update archived `START_HERE.md` for new work.

## Required checks

Run where practical:

```bash
scripts/rscript_env_caller.R -e "devtools::document()"
scripts/rscript_env_caller.R -e "devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::check(manual = FALSE)"
