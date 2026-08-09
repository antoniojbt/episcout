# Acceptance

Spec ID: `021-postgresql-identity-universe`

Status: Completed

- [x] SDD and TDD define the detailed contract before package-code changes.
- [x] Reviewed metadata declares at least two compatible PostgreSQL relations in one namespace.
- [x] Audit executes read-only and returns reconciled aggregate source, universe, overlap and issue tables.
- [x] Ordinary calculations and results do not collect identifiers into R.
- [x] Missing, blank, invalid, duplicate and collision conditions have deterministic documented semantics.
- [x] Materialisation repeats validation and publishes atomically only from a blocker-free contract.
- [x] The restricted table contains one canonical identifier per namespace under a unique constraint.
- [x] Existing registry, linkage and pseudonymisation interfaces remain compatible in focused and full local tests.
- [x] Unit and gated live PostgreSQL tests cover success, blockers, rollback, timeout/lock, empty sources and redaction.
- [x] Synthetic user documentation covers audit, blocked review, corrected contract, materialisation and the registry boundary.
- [x] Focused/full tests, PostgreSQL 17 integration, lint, R CMD check and `git diff --check` pass locally.
- [x] PR #219 passed macOS, Ubuntu, PostgreSQL integration, coverage, both Codecov gates and CodeFactor, then was accepted and merged into `master` on 2026-08-08, closing issue #215.
