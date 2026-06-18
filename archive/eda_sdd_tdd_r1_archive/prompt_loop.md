# Archived Prompt Loop Notes

This file used to hold reusable prompts for the original specification-first
EDA PR loop. That loop is now archived.

Use these files for historical context:

- `archive/eda_sdd_tdd_r1_archive/START_HERE.md`
- `archive/eda_sdd_tdd_r1_archive/codex/revised-pr-plan-tdd-first.md`
- `archive/eda_sdd_tdd_r1_archive/codex/tdd-first-codex-instructions.md`
- `archive/eda_sdd_tdd_r1_archive/codex/review-checklist.md`

Use the root-level future workspace for new work:

- `future/README.md`
- `future/TODOs.md`
- `future/specs/`

## Historical Pattern

The archived PR loop used this sequence:

```text
external fixture and expected output first
failing fixture-backed tests second
minimal implementation third
```

Future specs should preserve that pattern where fixture-backed behaviour is
being added or changed.
