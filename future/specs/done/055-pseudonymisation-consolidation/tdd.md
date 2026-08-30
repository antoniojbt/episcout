# Verification Design

Spec ID: `055-pseudonymisation-consolidation`
Status: Active

- Preserve exact outputs and no-warning behaviour for omitted identifier rules.
- Independently test identity, trim and trim-upper preparation, Unicode/case examples, null/blank refusal, regex failure, retained invalid flags and preparation collisions.
- Migrate populated and empty version-1 registries; reject partial/incompatible structures and changed preparation without partial writes.
- Import neutral mappings in audit/apply modes; prove tokens are preserved, conflicts are value-free, and rollback is complete.
- Instrument token generation above one batch; prove bounded calls, stable ordering, set-based collision checks, five-retry failure and no per-token database query.
- Verify semantic fingerprints are deterministic across row/order and batch-size changes and change for semantic configuration or source changes.
- Test least-privilege roles for every structured privilege issue, inherited ownership, replacement ownership, audit/apply parity and transactional recheck.
- Run focused tests, live disposable PostgreSQL integration, `scripts/check-local.sh`, `scripts/check-cran.sh`, workflow checks and `git diff --check` before release.
- Verify the installed synthetic guide with no private data, credentials or project paths.
