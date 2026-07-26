# Decisions

Record decisions that affect package scope, architecture, testing or review standards.

| Date | Decision | Rationale | Consequences |
|---|---|---|---|
| 2026-06-15 | Use root-level `future/` for next-phase planning. | `.Rbuildignore` already excludes `future/`, and it separates active planning from the archived SDD sequence. | Active SDD/TDD specs live outside `spec_driven_EDA_plan/`. |
| 2026-06-15 | Rename the completed SDD docs folder to `archive/eda_sdd_tdd_r1_archive/`. | The original control files are historical, not live instructions. | Archived files are references only. |
| 2026-06-15 | Keep executable tests under `tests/testthat/`. | Tests should run through normal R package tooling and CI. | `future/specs/*/tdd.md` defines test intent only. |
| 2026-06-15 | Use `scripts/rscript_env_caller.R` in future check commands. | The repo requires the project mamba R environment. | Future specs avoid bare `Rscript`. |
| 2026-07-25 | Treat `epi_stats_*` as the active main statistics layer and review specification-first EDA summaries for alignment and reuse before refactoring either interface. | The original EDA architecture says to build on existing helpers, while the current EDA implementation duplicates some logic and omits summary contracts for text, date and datetime variables. | Spec 007 performs a design-only contract review; package changes require a separately approved implementation spec. |

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
