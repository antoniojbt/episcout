# Acceptance

Spec ID: 051
Status: Completed

- Hand-authored fixtures cover balanced and unbalanced panels, entry, last observation, reappearance, duplicates, invalid identifiers, missing time and declared empty timepoints.
- Custom structure, follow-up, timepoint, missingness and change denominators reconcile exactly.
- Canonical summaries are literally identical to `epi_eda_profile_stratified()` on each backend, except the documented PostgreSQL Shapiro limitation.
- PostgreSQL runs preserve snapshot, rollback, connection reuse and identifier locality.
- Exact component/container/column order and typed zero-row schemas match the SDD for both backends.
- Truth covers absence versus present missingness, identical repeats versus conflict, first/last gaps, all-missing variables, one-occasion entities, signed adjacent/first-last deltas, `NaN`, positive and negative infinity, and zero eligibility.
- Invalid/missing/duplicate/reordered time declarations and unexpected observed time fail deterministically; a declared empty occasion remains visible.
- Every PostgreSQL custom component reconciles with the data-frame fixture; no test accepts a backend-specific legacy schema.
- Live PostgreSQL tests prove one RRRO snapshot under concurrent mutation, source/catalogue tamper detection, real-error rollback/reuse, aggregate-only queries and privacy-canary non-leakage.
- Existing ordinary, stratified, Table 1, reporting and #346–#348 interfaces remain unchanged.
- Parse, lint, focused data-frame and PostgreSQL tests, workflow-state, local/CRAN checks where applicable, hosted checks and `git diff --check` pass.

## Local evidence

- `EPISCOUT_TEST_POSTGRES=1 ... devtools::test(filter='eda-longitudinal', reporter='summary', stop_on_failure=TRUE)` passed every #346–#349 longitudinal suite against disposable PostgreSQL 17.
- The #349 PostgreSQL fixture retains hand-derived delta values and statistics, absence/missing/conflict truth, declared empty time, issue order, concurrent snapshot, source/catalogue change, real-error rollback/reuse and privacy-canary assertions.
- `lintr::lint('R/eda_longitudinal.R')` reported no lints.
- `scripts/check-workflow-state.sh --offline` passed with specification 051 active.
- `scripts/check-local.sh` passed: 0 errors, 0 warnings and the two known environment/repository notes.
- `scripts/check-cran.sh` passed: 0 errors, 0 warnings and three known incoming/time/`docs` notes.
- `git diff --check` passed.
- An independent read-only review rejected four successive schema/denominator/privacy gaps; all were corrected and the final review found no remaining blocker.

PR-356 passed hosted PostgreSQL integration, Ubuntu R CMD CHECK, coverage, CodeQL, CodeFactor and Codecov checks and merged as `0a5e3a8249a0b192bed489e2844e8b8b6cb87a0e`. Issue #349 closed automatically. Canonical post-merge CodeQL, coverage and R CMD CHECK (including PostgreSQL and Ubuntu) also passed.
