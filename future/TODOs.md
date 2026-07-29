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

- Use this template and place the task under the appropriate priority heading:

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

- For simple tasks only use e.g. `- [ ] xxx`
- Keep README and `NEWS.md` aligned with user-facing workflow changes.
- Move completed items to `changelog.md`.
- Check this `TODOs.md` file and `future/specs/` specs are aligned.

## Task list

### Priority 1

- [x] Review and accept the target contracts and ordered implementation recommendations from completed spec `007-eda-stats-alignment-review`; create spec 008 only after that human approval.
- [x] Implement spec `008-univariate-stats-eda-alignment`: shared univariate statistics cores, compatible public adapters and opt-in complete EDA v2 summaries.
- [x] Implement spec `009-repository-lint-style-cleanup`: remove the 163 genuine loaded-package lint findings and enforce the corrected lint policy locally and in CI.
- [ ] agent truth review with specific instructions pack
    - [ ] why are penguins and blood data not downloaded directly each time from the package itself. My concern is the agent may re-write them to fit tests given it recreated these fixtures.
    <!-- - [ ]  -->
- [ ] Human live walkthrough, no agent needed here (clone, install, follow vignettes).
- [ ] Carry out changes needed from human review

### Priority 2

- [ ] Add functions to create a data dictionary when real data already exists but does not have a dictionary.
- [ ] Sanitise dictionaries so that R, QGIS, SQL/MariaDB/postgreSQL can easily use them as input
- [ ] Add a follow-up pseudonymisation spec for PII identification, dataset
      rewriting, identifier removal, output validation and read-only raw-data
      files. Secure bridge-table v1 was completed in spec 005.
- [ ] add functions to load, connect, etc data into db. 

### Priority 3

- [ ] Review adoption after one compatibility release and decide when EDA summary version 2 should become the default; retain explicit version 1 until that decision is implemented in a numbered migration spec.
- [ ] check codecov percentage decrease
- [ ] Consider visual-regression strategy for EDA plots only after plot
      contracts are stable.
- [ ] Add biomedical EDA extensions as separate numbered specs.
- [ ] Revisit spec `003-large-data-backend-strategy` only after a concrete
      workload and performance target are defined.
