# START HERE: episcout SDD/TDD development guide

## Purpose

This file identifies the active documentation path for developing the new specification-first EDA layer in `episcout`.

Use this file to avoid confusion from earlier drafts and overlapping instruction files.

## Current judgement

The overall approach is sound:

```text
specification first
external real-data fixtures
independently computed expected outputs
failing tests
minimal implementation
small PRs
agent constrained by docs
```

This is good practice for epidemiology/data science package development with coding agents.

It reduces common agent-assisted coding risks:

```text
plausible but wrong code
tests written after implementation
tests that reproduce the implementation
large unreviewable PRs
silent behaviour changes
scope creep
```

The main risk is not the architecture. The main risk is documentation sprawl.

## Active canonical documents

Use these as the active repo documents:

```text
docs/sdd/0001-spec-first-eda.md
docs/sdd/mvp-scope.md
docs/sdd/data-dictionary-spec.md
docs/sdd/tdd-external-fixtures.md
docs/roadmap.md
docs/adr/0001-*.md
docs/adr/0005-external-fixture-tdd.md
docs/codex/tdd-first-codex-instructions.md
docs/codex/revised-pr-plan-tdd-first.md
docs/codex/review-checklist.md
docs/codex/update-existing-docs.md
```

## Superseded or non-canonical files

Do not use these as active Codex instructions:

```text
codex_instructions_for_episcout.md
episcout_tdd_external_fixtures.md
epi_ds_sdd_tdd_agent_workflow_summary.md
sdd_tdd_guideline_for_epi_ds_agent_workflows.md
docs/codex/pr-plan.md, unless clearly marked as superseded
```

Some of these may remain useful as background notes, but they should not control implementation.

## Immediate Codex starting point

Start Codex with this:

```text
Read AGENTS.MD and docs/START_HERE.md.

Then read:
- docs/sdd/0001-spec-first-eda.md
- docs/sdd/tdd-external-fixtures.md
- docs/codex/tdd-first-codex-instructions.md
- docs/codex/revised-pr-plan-tdd-first.md
- docs/codex/review-checklist.md

Then follow Instruction 1 in:

docs/codex/tdd-first-codex-instructions.md

Scope:
- documentation only
- no R code changes
- no DESCRIPTION changes
- no NAMESPACE changes
- no generated man/ files

Before making changes, summarise what you plan to edit.

Keep the change small and reviewable.
```

## Development principle

Do not ask Codex to implement the full workflow in one step.

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

## MVP discipline

The first MVP should remain small:

```text
blood_storage fixture
data dictionary/spec
expected schema output
expected missingness output
eda_spec()
check_schema()
profile_missing()
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

## Recommended immediate sequence

```text
1. Follow Instruction 1 in docs/codex/tdd-first-codex-instructions.md.
2. Follow Instruction 2 in docs/codex/tdd-first-codex-instructions.md.
3. Follow Instruction 3 in docs/codex/tdd-first-codex-instructions.md.
4. Add failing fixture-backed tests.
5. Implement only enough code to pass those tests.
```

## Review rule

Before merging any Codex PR, check:

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

## Final position

The project is following good practice.

It is not over-engineered in architecture.

It may become over-engineered if the repo keeps multiple overlapping instruction files without a clear canonical starting point.

This `START_HERE.md` file should be the entry point.
