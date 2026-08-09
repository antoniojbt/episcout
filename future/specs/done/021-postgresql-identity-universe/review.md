# Review Notes

Spec ID: `021-postgresql-identity-universe`

Status: Completed

## Findings

No unresolved local implementation finding. The public specification is deterministic and rejects extra value-bearing columns. Audit and materialisation share the same PostgreSQL-resident aggregate core; audit sets the transaction read-only, while materialisation repeats validation after bounded advisory-lock acquisition and creates and revokes the destination inside the protected transaction. Ordinary result and condition paths contain reviewed metadata, fixed prose and aggregates only.

## Open Questions

None. Version 1 deliberately fixes one namespace, identity normalisation and `existing = "error"`; broader behaviour requires a later reviewed specification.

## Closeout Notes

Independent set arithmetic for `{A, B, B, C}` and `{B, C, D}` establishes expected source distinct counts, duplicate excess, four-member union, two single-source members, two multi-source members, two-member intersection and directional coverage of `2 / 3`. The focused universe/linkage/pseudonymisation tests and full local suite pass with expected environment skips. Package-loaded lint is clean. `devtools::check(manual = FALSE)` completed with zero errors, warnings or notes, and `git diff --check` passed.

The live PostgreSQL test passed 42 expectations covering reconciled success, a read-only audit with no destination, restricted materialisation, unique constraint, `PUBLIC` revocation, existing-destination blocking, null/blank/invalid and empty-source handling, redaction, advisory-lock timeout and forced post-create rollback. The complete PostgreSQL 17 integration selection also passed the existing catalogue, EDA parity, source-boundary and longitudinal pseudonymisation suites; only the separately gated one-million-row benchmark was intentionally skipped. The mandatory PostgreSQL CI job selects the new test.

PR #219 passed macOS, Ubuntu, PostgreSQL integration, coverage, both Codecov gates and CodeFactor. Project coverage increased from 91.55% to 91.69%; the final patch-coverage gate passed at 93.88%. The repository owner merged the PR as `ec6134b` on 2026-08-08, closing issue #215. The completed record belongs under `future/specs/done/`.
