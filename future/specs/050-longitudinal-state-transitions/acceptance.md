# Acceptance

Spec ID: 050
Status: Active

- [x] The final #348 authority is represented in sdd.md.
- [x] The exact public interface, class, component order and typed schemas are implemented and documented.
- [x] Independent PostgreSQL truth proves all four two-state transition cells and the exact eligible denominator.
- [x] Missing, conflict, entry and exit treatment reconciles to the retained population.
- [x] Unavailable periods and pairs retain the frozen rows and typed unavailable values.
- [x] Domains are deterministic, complete, capped at 50 states and preflighted inside PostgreSQL.
- [x] Literal 51-level declaration, observation and adjacent union failures return no partial object.
- [x] One read-only repeatable-read snapshot covers all work; rollback and connection reuse are proved.
- [x] No identifier or row-level value appears in results, ordinary errors or logs.
- [x] Help, README, NEWS, project map and longitudinal vignette explain the generic boundary.
- [x] Focused parse, lint, unit, disposable PostgreSQL, workflow-state, full local and CRAN checks pass with recorded notes only.
- [x] Independent review has no unresolved substantive finding.
- [ ] Hosted required checks pass before merge.
- [ ] PR, merge commit, issue closure and successor are recorded only after canonical lifecycle completion.

## Current Evidence Boundary

The recovered implementation now passes focused unit and PostgreSQL 17 tests, workflow-state, full local and CRAN checks. Full checks report no errors or warnings; only the repository's known/environmental notes remain. Hosted checks and GitHub lifecycle evidence are still pending.
