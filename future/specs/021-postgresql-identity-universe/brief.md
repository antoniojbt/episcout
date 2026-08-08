# Brief

Spec ID: `021-postgresql-identity-universe`

Status: Accepted for implementation

Owner: repository-owner

## Problem

One reviewed identifier namespace may be distributed across several PostgreSQL tables. Users need reconciled source quality, distinct-universe, duplicate, pairwise-overlap and normalisation-collision evidence before they can deliberately publish a restricted canonical universe, without retrieving identifier values into R.

## Goal

Add a value-free metadata specification and an audit-first PostgreSQL workflow. Audit must run in one read-only repeatable-read snapshot. Materialisation must repeat the same checks in a protected write transaction, refuse every blocker and existing destination, and atomically create one restricted table containing namespace, canonical identifier and aggregate source-membership count.

## Non-goals

- Pseudonym or token generation, fuzzy linkage, entity resolution or automatic correction.
- Multiple namespaces, non-identity normalisation, destination replacement or a row-level membership table in version 1.
- Identifier-bearing R diagnostics, exports, grants, schemas, backup policy or disclosure control.
- Treating observed identifiers as confirmed people.

## Candidate Files

- `R/epi_sec_identity_universe.R`
- `tests/testthat/test-sec-identity-universe.R`
- `tests/testthat/test-sec-identity-universe-postgres.R`
- `vignettes/longitudinal-pseudonymisation.Rmd`
- `PROJECT_MAP.md`
- `NEWS.md`

## Risks

- A query or condition could leak an identifier or native database message.
- Audit and materialisation could observe different source states unless every materialisation check is repeated inside one snapshot.
- Normalisation or cross-type coercion could merge distinct identifiers.
- Destination races or partial publication could leave an unsafe table.
- Source-membership counts could be confused with row frequency or confirmed persons.
