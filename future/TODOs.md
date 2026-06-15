# TODOs

Keep all future work here with a priority order.
Do not create additional files such as backlog.md or equivalent.
Centralise task list here for clarity and for easy human review.

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

## Priority 1

- [ ] Decide which future spec to activate first.
- [ ] For the activated spec, record baseline package test/check status before
      package-code changes.
- [ ] Keep implementation work scoped to one numbered spec at a time.


## Priority 2

- [ ] Convert any new candidate work into a numbered spec before coding.
- [ ] Review fixture anti-circularity guardrails before adding new expected
      outputs.
- [ ] Keep README and `NEWS.md` aligned with user-facing workflow changes.



## Priority 3

- [ ] Revisit large-data backend implementation after the design spec is
      accepted.
- [ ] Consider visual-regression strategy for EDA plots only after plot
      contracts are stable.
- [ ] Add biomedical EDA extensions as separate numbered specs.
