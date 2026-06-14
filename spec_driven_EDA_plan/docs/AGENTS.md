# AGENTS.md

This file is specifically for work outside of the main branch.
It is for work contained only within this directory, `spec_driven_EDA_plan`.

## Scope

This repository is an R package. Follow the specification-first EDA plan.

## Required reading

Before editing files, read:

- spec_driven_EDA_plan/docs/START_HERE.md
- spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md
- spec_driven_EDA_plan/docs/codex/review-checklist.md

Treat START_HERE.md as the live source of truth.

## Working rules

- Complete only the active PR-sized task.
- Do not broaden scope.
- Do not perform unrelated refactoring.
- Respect must-edit and must-not-edit lists.
- Do not use episcout functions to generate expected fixture outputs.
- Use external fixtures and independently computed expected outputs.
- Update START_HERE.md at closeout.

## Required checks

Run where practical:

```bash
Rscript -e "devtools::document()"
Rscript -e "devtools::test(reporter = 'summary')"
Rscript -e "devtools::check(manual = FALSE)"
R CMD check --no-manual .
