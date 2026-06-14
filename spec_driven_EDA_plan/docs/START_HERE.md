# START HERE: episcout SDD/TDD development guide

This `START_HERE.md` file is the entry point.

## Purpose

This file identifies the active documentation path for developing the new specification-first EDA layer in `episcout`.


## Immediate Codex starting point

Start Codex with this:

```text
Read AGENTS.MD and spec_driven_EDA_plan/docs/START_HERE.md.

Then read:
- spec_driven_EDA_plan/docs/sdd/0001-spec-first-eda.md
- spec_driven_EDA_plan/docs/sdd/tdd-external-fixtures.md
- spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md
- spec_driven_EDA_plan/docs/codex/revised-pr-plan-tdd-first.md
- spec_driven_EDA_plan/docs/codex/review-checklist.md

Then follow Instruction 1 in:

spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md

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
1. Follow Instruction 1 in spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md.
2. Follow Instruction 2 in spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md.
3. Follow Instruction 3 in spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md.
4. Add failing fixture-backed tests.
5. Implement only enough code to pass those tests.
```

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
