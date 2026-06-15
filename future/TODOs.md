# TODOs

- Keep all future work here with a priority order.
- Do not create additional files such as `backlog.md` or equivalent.
- Centralise task list here for clarity and for easy human review.
- Decide which future spec to activate first.
- Convert any new candidate work into a numbered spec before coding.
- If a task in this file does not have the needed SDD-TDD approach and a numbered spec, prompt me to clarify and create. Tasks that require multiple steps, substantial judgement calls, or may compromise existing code must have an SDD-TDD specification. If unclear ask first before proceeding with any write work.
- For the activated spec, record baseline package test/check status before package-code changes.
- Keep implementation work scoped to one numbered spec at a time.
- Review fixture anti-circularity guardrails before adding new expected outputs.
- Keep README and `NEWS.md` aligned with user-facing workflow changes.

Use this template and place the task under the appropriate priority heading:

```markdown
- Short title:
    - Problem:
    - Goal:
    - User need:
    - Proposed scope:
    - Out of scope:
    - Candidate files:
    - Risks:
    - Suggested spec ID:
```

For simple tasks only use e.g. `- [ ] xxx`

## Task list

### Priority 1

- Short title: Address senior R package review follow-ups
    - Problem: The review in `future/reviews/2026-06-15-senior-r-package-review.md` found API-surface, test-design, EDA edge-case and dependency-guard risks.
    - Goal: Convert the review findings into a scoped SDD/TDD implementation plan before package-code changes.
    - User need: Keep the package maintainable and CRAN-ready without widening the current branch ad hoc.
    - Proposed scope: Create a numbered spec for the highest-value findings: EDA summary edge cases, missing-code semantics, non-circular fixture expectations, public API cleanup and optional dependency guards.
    - Out of scope: Large-data backend implementation and unrelated helper rewrites.
    - Candidate files: `future/reviews/2026-06-15-senior-r-package-review.md`, `future/specs/`, `R/eda_*`, `R/epi_plot_parallel.R`, `R/utils-tidy-eval.R`, `tests/testthat/`.
    - Risks: API cleanup may be breaking if current users rely on accidental exports.
    - Suggested spec ID: `004-senior-review-followups`


- [ ] get instructions to download, install, episcout2 branch
- [ ] human live walkthrough, no agent needed here
- [ ] carry out changes needed from human review

- [ ] run devtools and CRAN checks
- [ ] merge with main
- [ ] tag v0.2.0


### Priority 2

- [ ] Add functions to create a data dictionary when real data already exists but does not have a dictionary.
- [ ] Sanitise dictionaries so that R, QGIS, SQL/MariaDB/postgreSQL can easily use them as input
- [ ] Revisit large-data backend implementation after the design spec is
      accepted.

### Priority 3

- [ ] Consider visual-regression strategy for EDA plots only after plot
      contracts are stable.
- [ ] Add biomedical EDA extensions as separate numbered specs.

## Done

- [x] thorough code review, use prompt in `future/prompts/senior-r-package-review.md`
  - Review written to `future/reviews/2026-06-15-senior-r-package-review.md`.
  - Corrected the confirmed `epi_stats_numeric()` kurtosis documentation issue.
  - Planned follow-up work as `004-senior-review-followups` candidate.
- [x] Completed spec: `001-phase-1-helper-stabilization`
  - Baseline recorded before package-code changes.
  - TDD edge-case tests added before implementation.
  - Implementation scoped to `epi_stats_numeric()` and `epi_stats_na_perc()`.
  - Final `devtools::test(reporter = 'summary')` passed.
- [x] check future work has been setup properly for SDD-TDD:
  - PR14
  - add penguins_raw
  - ?
