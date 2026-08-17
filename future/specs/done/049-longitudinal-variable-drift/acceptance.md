# Acceptance

Spec ID: `049`
Status: Completed

- [x] The frozen #347 authority is reproduced before package edits.
- [x] The exact public API and class/component order are implemented and documented.
- [x] Schema and missingness cover all selected fields; numeric, categorical and temporal fields use only canonical summary semantics.
- [x] Exact period/adjacent schemas, ordering, numerator/denominator rules and unavailable values meet decisions 4–10 in `sdd.md`.
- [x] Categorical preflight is bounded at `max_levels + 1`; any over-limit domain, declaration or union fails without an object.
- [x] One read-only repeatable-read snapshot covers validation, preflight, summaries and assembly; success and failure leave the connection reusable.
- [x] Tests establish hand-derived truth, canonical reconciliation, snapshot isolation, rollback and no identifier leakage.
- [x] British-English help, README and longitudinal documentation state the descriptive and data-locality boundary.
- [x] Focused parse/lint/unit/PostgreSQL, workflow-state and `scripts/check-local.sh` checks pass or have exact recorded environmental exceptions.
- [x] Review notes are recorded in `review.md`.
- [x] PR-352 passed hosted PostgreSQL, Ubuntu, coverage and CodeFactor checks and merged to canonical `master` as `commit-b4eddf0`.
- [x] Issue-347 closed automatically; issue-348 remains the explicit state-transition successor.
- [x] The manifest is completed and this specification is retained under `future/specs/done/`.

## Current evidence boundary

Focused unit and disposable PostgreSQL tests cover the implemented contract, including exact typed schemas, hand-derived values, reconciliation against canonical single-period summaries, a literal 51-level refusal, a concurrent write during the public call, rollback/reuse after failure, and sanitisation of a real PostgreSQL error containing a private marker. Workflow-state and `scripts/check-local.sh` also pass; R CMD check reports only the repository's known `.git`, clock and `docs` notes (zero errors and zero warnings). Hosted PostgreSQL integration, Ubuntu, coverage and CodeFactor passed before canonical merge.
