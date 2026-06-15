# Decisions

Record decisions that affect package scope, architecture, testing or review
standards.

| Date | Decision | Rationale | Consequences |
|---|---|---|---|
| 2026-06-15 | Use root-level `future/` for next-phase planning. | `.Rbuildignore` already excludes `future/`, and it separates active planning from the archived SDD sequence. | Active SDD/TDD specs live outside `spec_driven_EDA_plan/`. |
| 2026-06-15 | Rename the completed SDD docs folder to `archive/eda_sdd_tdd_r1_archive/`. | The original control files are historical, not live instructions. | Archived files are references only. |
| 2026-06-15 | Keep executable tests under `tests/testthat/`. | Tests should run through normal R package tooling and CI. | `future/specs/*/tdd.md` defines test intent only. |
| 2026-06-15 | Use `scripts/rscript_env_caller.R` in future check commands. | The repo requires the project mamba R environment. | Future specs avoid bare `Rscript`. |

## Decision Template

```markdown
## YYYY-MM-DD - <decision>

- Context:
- Options considered:
- Decision:
- Rationale:
- Consequences:
- Review date:
```
