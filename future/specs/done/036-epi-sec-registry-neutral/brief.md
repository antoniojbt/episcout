# Brief

Spec ID: `036-epi-sec-registry-neutral`
Status: Review

## Problem

The stable identity-registry path treated PostgreSQL ownership and `PUBLIC` privileges as package policy, mutated configured grants during registry and output creation, and reported a governance-style `blocked` result with `schema_restricted`. Those behaviours are not required to validate the registry structure, keep token settings immutable or perform a requested atomic pseudonymisation write.

## Objective

Implement `issue-284` as the second behaviour slice from completed `spec-034`. Registry inspection and initialisation must be structural and privilege-neutral, PostgreSQL must determine access through ordinary server outcomes, and the path must retain the stable registry, atomicity, locking, rollback, source/registry/output separation, ownership-safe destructive replacement and fixed value-free database failures.

## Observable Outcome

`epi_sec_identity_registry_init()` returns a registry result without `schema_restricted`, uses `incompatible` only for structural incompatibility and neither inspects nor changes PostgreSQL privileges. The stable-registry path in `epi_sec_pseudonymise_db()` likewise performs no privilege query or grant mutation for registry, crosswalk or output objects, while configured grants remain unchanged and insufficient permissions produce sanitised technical errors.

## Scope

- Registry inspection, initialisation, result construction and printing in `R/epi_sec_registry.R`.
- Privilege queries and mutations reached by the stable-registry pseudonymisation path in `R/epi_sec_pseudonymise_db.R`.
- Focused offline and live PostgreSQL tests, generated help and only directly affected guide text.
- `spec-036` and normal lifecycle records.

## Exclusions

- Linkage metadata, pseudonymisation result statuses, issue severities, diagnostic values and manifest fields reserved for `issue-285`.
- Identity-universe behaviour completed by `issue-278`/`spec-035`.
- Pseudonym generation, identity semantics, registry structure/version, new dependencies, real identifiers, production databases, role-administration features or a broad longitudinal guide rewrite.

## Recovery

The change introduces no data migration and changes no registry row. Registry creation and pseudonymisation remain transactionally atomic; failed writes roll back, and the scoped commits can be reverted without deleting registry or output data.
