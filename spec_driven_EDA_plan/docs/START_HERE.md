# START HERE: episcout SDD/TDD development guide

This is the live control panel for the specification-first EDA work in
`episcout`.

Use this file first to see:

- current status;
- the active PR-sized task;
- which instruction block to follow;
- files that are in and out of scope;
- expected test state;
- required closeout updates.

Design rationale lives in the SDD and ADR files. The full PR sequence lives in
`spec_driven_EDA_plan/docs/codex/revised-pr-plan-tdd-first.md`. This file is the
operational source of truth for what happens next.

## Current Status

| Work item | Status | Next action |
|---|---|---|
| Phase 0 documentation baseline | Done | Keep docs in sync as scope changes. |
| PR 1 external fixture files | Done | Fixture files are present under `tests/testthat/fixtures/blood_storage/`. |
| PR 2 fixture-backed failing tests | Done | Tests are present and may fail until PR 3 implements the missing functions. |
| PR 3 spec, schema and missingness implementation | Done | Spec, schema and missingness functions are implemented. |
| PR 4 synthetic-data failing tests | Done | Tests are present and may fail until PR 5 implements `generate_synthetic_data()`. |
| PR 5 synthetic-data implementation | Done | `generate_synthetic_data()` is implemented. |
| PR 6 summary and plot failing tests | Done | Tests are present and may fail until PR 7 implements `profile_summaries()` and `profile_plots()`. |
| PR 7 summary and plot implementation | Done | `profile_summaries()` and `profile_plots()` are implemented. |
| PR 8 run_eda failing tests | Done | Tests are present and may fail until PR 9 implements `run_eda()`. |
| PR 9 run_eda implementation | Done | `run_eda()` is implemented. |
| PR 10 report-template tests | Done | Report-rendering tests are present and may fail until PR 11 implements `render_eda_report()`. |
| PR 11 report rendering implementation | Done | `render_eda_report()` and the bundled Quarto report template are implemented. |
| PR 12 project template work | Active | Follow Instruction 10 in `spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md` and add the project template from the PR plan. |

## Active PR

```text
PR 12: Add project template
```

Instruction:

```text
Follow Instruction 10 in:
spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md
```

Required reading:

```text
AGENTS.MD
spec_driven_EDA_plan/docs/START_HERE.md
spec_driven_EDA_plan/docs/sdd/0001-spec-first-eda.md
spec_driven_EDA_plan/docs/sdd/data-dictionary-spec.md
spec_driven_EDA_plan/docs/sdd/tdd-external-fixtures.md
spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md
spec_driven_EDA_plan/docs/codex/revised-pr-plan-tdd-first.md
spec_driven_EDA_plan/docs/codex/review-checklist.md
```

## PR 12 Scope

Must edit:

```text
inst/project-template/
spec_driven_EDA_plan/docs/START_HERE.md
```

May read:

```text
R/eda_report.R
inst/report-template/eda.qmd
tests/testthat/test-eda_report.R
spec_driven_EDA_plan/docs/codex/revised-pr-plan-tdd-first.md
```

Must not edit:

```text
R/eda_spec.R
R/eda_schema.R
R/eda_missing.R
R/eda_synthetic.R
R/eda_summaries.R
R/eda_plots.R
R/run_eda.R
R/eda_report.R
inst/report-template/eda.qmd
```

Expected test state:

```text
PR 12 should add project-template coverage without regressing PR 11 report tests.
```

## Closeout Rule

Every PR must update this file before it is considered complete.

At closeout:

```text
1. Mark the completed PR as Done.
2. Mark the next PR as Active.
3. Update the Active PR section.
4. Update must-edit and must-not-edit lists.
5. Record whether failing tests are expected.
6. Leave SDD and ADR files unchanged unless the design changed.
```

If a PR intentionally does not update `START_HERE.md`, record that exception in
the PR description.

## Development Principle

Use this order:

```text
external fixture and expected output first
failing fixture-backed tests second
minimal implementation third
```

For every new specification-first EDA function, use:

```text
fixture data
+ fixture specification
+ independently computed expected output
-> failing tests
-> implementation
```

## Source of Truth

- Live status and next action: `spec_driven_EDA_plan/docs/START_HERE.md`
- Executable Codex instructions: `spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md`
- Full PR sequence: `spec_driven_EDA_plan/docs/codex/revised-pr-plan-tdd-first.md`
- Design rationale: `spec_driven_EDA_plan/docs/sdd/0001-spec-first-eda.md`
- Fixture TDD rationale: `spec_driven_EDA_plan/docs/sdd/tdd-external-fixtures.md`
- Review checklist: `spec_driven_EDA_plan/docs/codex/review-checklist.md`

## Out of Scope Until Later PRs

Do not start with:

```text
Arrow
DuckDB
project templates
multiple fixtures
large-data optimisation
full plot/report system
```

These belong in later phases.

## Review Rule

Before merging any PR, check:

```text
Does this PR do only one thing?
Did it change unrelated files?
Did it remove or rename existing epi_* functions?
Did it add tests for new behaviour?
Did it update roxygen docs for exported functions?
Did it add heavy dependencies unnecessarily?
Does it preserve backwards compatibility?
Are expected outputs independently computed?
Are synthetic-data outputs clearly labelled where relevant?
Was START_HERE.md updated for the next PR?
```
