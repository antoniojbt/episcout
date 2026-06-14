# START HERE: episcout SDD/TDD development guide

This is the operational entry point for the specification-first EDA work in
`episcout`.

## Purpose

Use this file to see:

- what has already been planned;
- what should happen next;
- what is out of scope for the immediate work;
- which Codex instruction block to follow.

Detailed design rationale lives in the SDD and ADR files. This file should stay
short and action-oriented.

## Current status

| Work item | Status | Next action |
|---|---|---|
| Phase 0 documentation baseline | Done | Keep docs in sync as scope changes. |
| PR 1 external fixture files | Next | Follow Instruction 1 in `spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md`. |
| PR 2 fixture-backed failing tests | Not started | Add tests after PR 1 fixtures exist. |
| PR 3 spec, schema and missingness implementation | Not started | Implement only after PR 2 tests exist. |
| Later synthetic, summary, plot, report and template work | Not started | Continue through the PR plan after PR 3. |

## Immediate Codex starting point

For the next PR, start Codex with this:

```text
Read AGENTS.MD and spec_driven_EDA_plan/docs/START_HERE.md.

Then read:
- spec_driven_EDA_plan/docs/sdd/0001-spec-first-eda.md
- spec_driven_EDA_plan/docs/sdd/tdd-external-fixtures.md
- spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md
- spec_driven_EDA_plan/docs/codex/revised-pr-plan-tdd-first.md
- spec_driven_EDA_plan/docs/codex/review-checklist.md

Then follow Instruction 1 in spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md.

Scope:
- add external fixture files only
- no new package behaviour
- no DESCRIPTION changes unless a Suggests entry is needed for fixture regeneration
- no NAMESPACE changes
- no generated man/ files

Before making changes, summarise what you plan to edit.

Keep the change small and reviewable.
```

## Active development principle

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
→ failing tests
→ implementation
```

PR 2 may intentionally introduce failing tests. CI does not need to stay green
for that red-test PR. PR 3 is expected to implement the functions needed to make
those tests pass.

## Immediate scope

The next PR should include only:

```text
blood_storage fixture
data dictionary/spec
expected schema output
expected missingness output
fixture provenance
fixture regeneration script
```

Do not start with:

```text
Arrow
DuckDB
Quarto report
project templates
multiple fixtures
large-data optimisation
full plot/report system
```

These belong in later phases.

## Source of truth for next steps

- Executable Codex instructions: `spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md`
- Full PR sequence: `spec_driven_EDA_plan/docs/codex/revised-pr-plan-tdd-first.md`
- Design rationale: `spec_driven_EDA_plan/docs/sdd/0001-spec-first-eda.md`
- Fixture TDD rationale: `spec_driven_EDA_plan/docs/sdd/tdd-external-fixtures.md`
- Review checklist: `spec_driven_EDA_plan/docs/codex/review-checklist.md`

## Review rule

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
```
