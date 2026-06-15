# Refactor Review Prompt

Review this refactor for scope control and maintainability.

Check:

- exported function names and argument names remain stable unless explicitly
  changed by the active spec;
- unrelated modernization is not included;
- helper extraction reduces real duplication or complexity;
- tests protect public behaviour before implementation changes;
- docs and examples still match behaviour.
