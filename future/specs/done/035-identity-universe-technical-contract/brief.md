# Brief

Spec ID: `035-identity-universe-technical-contract`
Status: Completed

## Problem

The retained identity-universe workflow currently treats a confirmation field, restricted destination schema and `PUBLIC` privilege mutation as package authority, and uses `blocked`/`blocking` results for both technical inspection and write refusal. Those policy semantics are not required to calculate an exact identifier universe or to protect its transaction, uniqueness and value-free database boundary.

## Objective

Implement `issue-278` as the first behaviour slice from completed `spec-034`. Version-2 source metadata must be exactly technical and value-free, audit must report technical findings without becoming permission, materialisation must commit only after all error-severity checks pass, and PostgreSQL must determine access from its configured privileges without package grant inspection or mutation.

## Observable Outcome

`epi_sec_identity_universe_spec()` returns deterministic version-2 objects with five source columns and a bounded syntax-only adapter for the six-column development form. `epi_sec_identity_universe_db()` returns `audit_complete` after every completed audit, `not_written` for expected technical no-commit outcomes and `complete` only after commit, while preserving exact aggregate set calculations, source/type/collation checks, locks, rollback and fixed value-free errors.

## Scope

- The two retained identity-universe exports, their internal helpers and S3 print methods.
- Focused offline and live PostgreSQL tests.
- Roxygen-generated help and only the directly affected identity-universe guide section.
- Spec 035 and normal active-lifecycle records.

## Exclusions

- Linkage, stable registry, pseudonymisation and their current privilege/governance schemas.
- New identifier normalisation, value diagnostics, replacement, enrolment, pseudonym generation, backends or dependencies.
- Real identifiers, production data/databases, credentials, privacy classification, disclosure decisions or broad guide revision.

## Recovery

The change introduces no data migration and never replaces a destination. A failed materialisation remains transactionally rolled back; the scoped commit can be reverted without changing existing PostgreSQL objects or registry rows.
