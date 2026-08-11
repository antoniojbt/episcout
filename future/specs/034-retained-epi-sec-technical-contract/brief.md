# Brief

Spec ID: `034-retained-epi-sec-technical-contract`
Status: Active design inventory

## Problem

The seven maintained `epi_sec_*` exports combine useful pseudonymisation, registry, exact-linkage and identifier-universe mechanics with package decisions about confirmation, privacy classification, restricted schemas, PostgreSQL `PUBLIC` privileges, diagnostic redaction and permission to proceed. Removing the policy layer without an exact inventory could also remove controls that prevent an incorrect identity mapping, ambiguous duplicate result, token collision, partial database write or credential-bearing database error.

## Objective

Define an implementation-ready technical contract in which callers select transformations and diagnostics, PostgreSQL enforces its configured permissions, and episcout validates only the structure and database state needed to calculate or write the requested result correctly. Preserve every public export and the transaction, locking, uniqueness, reconciliation, exact-matching and cryptographic invariants.

## Outcome

The retained interface uses explicit output actions rather than privacy classes, treats audit as an optional inspection mode, reports technical findings without `blocked` or `blocking` policy terms, does not refuse or rewrite PostgreSQL privileges based on package policy, and returns requested identifier diagnostics as ordinary data. Three ordered implementation slices keep the standalone identity-universe change separate from core registry privilege behaviour and the tightly coupled linkage/pseudonymisation schema migration.

## Evidence Boundary

This design is based on current `master` source, generated help, focused unit tests, opt-in PostgreSQL test definitions, the longitudinal vignette, the installed database walkthrough, repository callers, release tag `0.3.0`, completed specifications 016/021/030, and live issue decisions #269/#274/#275/#276. Repository artefacts establish existing behaviour and compatibility exposure; owner issue #274 and correction #275 establish the intended authority boundary.

## Non-goals

- No package source, executable test, generated help, NAMESPACE, vignette, example or package behaviour change in issue #276.
- No export removal, cryptographic redesign, fuzzy/probabilistic linkage, new backend, schema creation, role administration, backup management or Epidepot work.
- No decision about institutional privacy policy, lawful processing, anonymity, disclosure control, sharing or publication.
- No inference that a column or value is personal, sensitive, safe or unsafe from its name, type, value or observed pattern.
- No broad rewrite of `vignettes/longitudinal-pseudonymisation.Rmd`; issue #269 remains paused until the implementation contract is stable.

## Successor

`first-implementation-issue.md` is the complete first tracker draft. The authorised dispatcher or owner must create that GitHub issue and record its number as this design's successor before issue #276 can complete. Later implementation trackers should be created only at the boundaries recorded in `sdd.md`.
