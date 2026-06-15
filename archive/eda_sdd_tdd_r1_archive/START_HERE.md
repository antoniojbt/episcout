# ARCHIVED: episcout SDD/TDD development guide

This file is historical. The original specification-first EDA PR sequence has
been completed or moved into the root `future/` workspace for later work.

Do not use this file as the live control panel for new work. Use:

```text
future/README.md
future/backlog.md
future/specs/
```

The archived SDD, ADR, roadmap and Codex instruction files remain useful as
design references.

This was the live control panel for the specification-first EDA work in
`episcout`.

This file originally tracked:

- current status;
- the active PR-sized task;
- which instruction block to follow;
- files that are in and out of scope;
- expected test state;
- required closeout updates.

Design rationale lives in the SDD and ADR files. The full historical PR
sequence lives in
`archive/eda_sdd_tdd_r1_archive/codex/revised-pr-plan-tdd-first.md`.

## Current Status

| Work item | Status | Next action |
|---|---|---|
| Phase 0 documentation baseline | Done | Keep docs in sync as scope changes. |
| PR 1 external fixture files | Done | Fixture files are present under `tests/testthat/fixtures/blood_storage/`. |
| PR 2 fixture-backed failing tests | Done | Tests are present and may fail until PR 3 implements the missing functions. |
| PR 3 spec, schema and missingness implementation | Done | Spec, schema and missingness functions are implemented. |
| PR 4 synthetic-data failing tests | Done | Tests are present and may fail until PR 5 implements `epi_eda_generate_synthetic_data()`. |
| PR 5 synthetic-data implementation | Done | `epi_eda_generate_synthetic_data()` is implemented. |
| PR 6 summary and plot failing tests | Done | Tests are present and may fail until PR 7 implements `epi_eda_profile_summaries()` and `epi_eda_profile_plots()`. |
| PR 7 summary and plot implementation | Done | `epi_eda_profile_summaries()` and `epi_eda_profile_plots()` are implemented. |
| PR 8 epi_eda_run failing tests | Done | Tests are present and may fail until PR 9 implements `epi_eda_run()`. |
| PR 9 epi_eda_run implementation | Done | `epi_eda_run()` is implemented. |
| PR 10 report-template tests | Done | Report-rendering tests are present and may fail until PR 11 implements `epi_eda_render_report()`. |
| PR 11 report rendering implementation | Done | `epi_eda_render_report()` and the bundled Quarto report template are implemented. |
| PR 12 project-template contract tests | Done | Failing tests define the project template and `epi_eda_create_project()` contract. |
| PR 13 project-template implementation | Done | `inst/project-template/` and `epi_eda_create_project()` are implemented. |
| PR 14 large-data design note | Moved to future | See `future/specs/003-large-data-backend-strategy/`. |

## Archived Active PR

There is no active PR in this archived workflow. The last open design thread
was moved to:

```text
future/specs/003-large-data-backend-strategy/
```

Required reading:

```text
AGENTS.MD
archive/eda_sdd_tdd_r1_archive/START_HERE.md
archive/eda_sdd_tdd_r1_archive/sdd/0001-spec-first-eda.md
archive/eda_sdd_tdd_r1_archive/sdd/data-dictionary-spec.md
archive/eda_sdd_tdd_r1_archive/sdd/tdd-external-fixtures.md
archive/eda_sdd_tdd_r1_archive/codex/tdd-first-codex-instructions.md
archive/eda_sdd_tdd_r1_archive/codex/revised-pr-plan-tdd-first.md
archive/eda_sdd_tdd_r1_archive/codex/review-checklist.md
```

## Historical PR 14 Scope

Must edit:

```text
archive/eda_sdd_tdd_r1_archive/sdd/large-data-backend-strategy.md
archive/eda_sdd_tdd_r1_archive/START_HERE.md
```

May read:

```text
archive/eda_sdd_tdd_r1_archive/sdd/0001-spec-first-eda.md
archive/eda_sdd_tdd_r1_archive/sdd/mvp-scope.md
archive/eda_sdd_tdd_r1_archive/adr/0003-r-not-pure-base-r.md
archive/eda_sdd_tdd_r1_archive/roadmap.md
archive/eda_sdd_tdd_r1_archive/repository-audit.md
archive/eda_sdd_tdd_r1_archive/codex/revised-pr-plan-tdd-first.md
archive/eda_sdd_tdd_r1_archive/codex/tdd-first-codex-instructions.md
```

Must not edit:

```text
R/epi_eda_spec.R
R/eda_schema.R
R/eda_missing.R
R/eda_synthetic.R
R/eda_summaries.R
R/eda_plots.R
R/epi_eda_run.R
R/eda_report.R
inst/report-template/eda.qmd
inst/project-template/
NAMESPACE
man/
```

Expected test state:

```text
PR 14 is documentation-only. No failing tests are expected, and no large-data
backend implementation should be added.
```

## Historical Closeout Rule

This rule applied while the archived workflow was active.

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

## Historical References

- Archived status and next action: `archive/eda_sdd_tdd_r1_archive/START_HERE.md`
- Executable Codex instructions: `archive/eda_sdd_tdd_r1_archive/codex/tdd-first-codex-instructions.md`
- Full PR sequence: `archive/eda_sdd_tdd_r1_archive/codex/revised-pr-plan-tdd-first.md`
- Design rationale: `archive/eda_sdd_tdd_r1_archive/sdd/0001-spec-first-eda.md`
- Fixture TDD rationale: `archive/eda_sdd_tdd_r1_archive/sdd/tdd-external-fixtures.md`
- Review checklist: `archive/eda_sdd_tdd_r1_archive/codex/review-checklist.md`

## Out of Scope Until Later PRs

Do not start with:

```text
Arrow
DuckDB
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
