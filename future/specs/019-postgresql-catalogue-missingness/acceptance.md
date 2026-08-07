# Acceptance

Spec ID: `019-postgresql-catalogue-missingness`
Status: Implemented

- [x] Non-missing catalogue values and PostgreSQL NULL counts have distinct,
  stable result schemas.
- [x] `values` contains no missing `source_value` and is bounded by `max_levels`
  per profiled column.
- [x] `missing` contains one exact `n_missing` row per selected column, including
  empty and all-NULL sources.
- [x] Existing dictionary eligibility, privacy and connection gates remain.
- [x] Unit and mandatory PostgreSQL 17 tests cover zero rows, all NULL, exact
  limit plus NULL and limit plus one.
- [x] Help, longitudinal guidance, the runnable walkthrough and NEWS explain
  conversion and compatibility.
- [x] Focused tests, lint, full tests, local checks and `git diff --check` pass;
  the live PostgreSQL test is discovered locally and awaits mandatory CI.
- [x] Spec 020 is reconciled as merged and completed, while roadmap item 3 is
  not started.
- [x] Mandatory PostgreSQL 17 CI passes the zero-row, all-NULL, exact-limit and
  over-limit live cases.
- [ ] Pull-request CI passes and the owner accepts and merges the implementation.
