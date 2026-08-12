# Brief

Spec ID: `036-epi-sec-registry-neutral`
Status: Active

## Objective

Make stable registry initialisation a neutral PostgreSQL technical operation. Preserve the versioned schema, immutable token settings, atomic creation, locking and rollback while removing package inspection or mutation of PostgreSQL PUBLIC privileges and policy-bearing registry result fields.

## Scope

- `epi_sec_identity_registry_init()` and its result/inspection helpers.
- Focused live PostgreSQL registry regression coverage.
- Directly affected roxygen and vignette wording.

## Exclusions

Linkage, pseudonymisation, identity-universe, role/grant administration, new dependencies and real identifiers remain out of scope.
