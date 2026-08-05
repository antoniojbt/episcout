# Brief

Spec ID: `016-longitudinal-pseudonymisation`
Status: Implemented
Owner: Repository owner

## Problem

The released `epi_sec_pseudonym()` helper creates pseudonyms for one in-memory vector, but the package has no reviewed, persistent linkage contract for multiple PostgreSQL tables. Analysts therefore lack a package-supported way to retain one stable pseudonymous identity across longitudinal records, exact identifier namespaces and reviewed aliases while keeping source identifiers out of portable configuration, ordinary R results and analysis relations.

## Goal

Add a configuration-driven PostgreSQL workflow that maintains a stable restricted identity registry, links exactly one declared identifier column per selected source table, supports repeated longitudinal records and reviewed many-alias-to-one-entity resolution, and writes pseudonymised copies into a separate restricted schema. The workflow must audit before writing, preserve source relations unchanged, return expected governance/data findings as actionable redacted blockers, and apply all writes atomically under deterministic concurrency control.

## User Outcome

A first-time user can find one friendly guide, create and review value-free linkage metadata, ask a database administrator to prepare suitable restricted schemas and privileges, audit registry and pseudonymisation requirements without writes, deliberately apply the workflow, verify stable tokens across runs and tables, understand duplicate or unmatched-identity blockers, and pass the resulting reviewed output dictionary into the EDA workflow. The documentation repeatedly states that pseudonymised data remain restricted personal data and are not anonymous or automatically disclosure-controlled.

## Scope

- Preserve the released behaviour of `epi_sec_pseudonym()` and replace any project-like examples with unmistakably synthetic neutral examples.
- Add `epi_sec_linkage_scaffold()` to derive draft table metadata from a reviewed multi-table dictionary without reading row values.
- Add `epi_sec_linkage_spec()` to validate confirmed table, record-key and database-resident crosswalk metadata.
- Add `epi_sec_identity_registry_init()` with read-only audit and deliberate apply modes for one restricted PostgreSQL identity registry.
- Add `epi_sec_pseudonymise_db()` with read-only audit and atomic apply modes for exact, reviewed multi-table linkage and pseudonymised output.
- Support PostgreSQL text-like, integral and UUID identifier families with exact family-aware identity semantics.
- Allow exactly one selected table to enrol entities, without assuming one row per entity.
- Support exact same-namespace identifiers and confirmed database-resident crosswalks, including reviewed many-alias-to-one-entity linkage.
- Separate identity resolution from longitudinal record-key and exact projected-duplicate handling.
- Use the confirmed multi-table dictionary as the privacy and output-column authority.
- Keep source identifiers and record-key values inside PostgreSQL by default and make all ordinary issues, audit records, conditions, manifests and printing value-free.
- Add synthetic unit and opt-in live PostgreSQL integration tests, a pinned PostgreSQL CI service, generated help, NEWS, README links and a dedicated vignette.

## Non-goals

File-backed registries, generic DBI backends, database views, schema creation by the workflow, role grants, backup management, PostgreSQL/server/driver/administrator log configuration, composite identity columns, fuzzy or probabilistic matching, phonetic matching, automatic PII detection, free-text inspection, arbitrary derivation rules, conflicting-record aggregation, automatic disclosure control, publication-safety claims, release, or tag creation.

## Public Interfaces

```r
epi_sec_linkage_scaffold(dictionary, tables = NULL)

epi_sec_linkage_spec(tables, record_keys = NULL, crosswalks = NULL)

epi_sec_identity_registry_init(
  con,
  registry_schema,
  token_prefix = "E",
  n_bytes = 24,
  mode = c("audit", "apply")
)

epi_sec_pseudonymise_db(
  con,
  dictionary,
  linkage,
  registry_schema,
  output_schema,
  catalogues = NULL,
  mode = c("audit", "apply"),
  token_column = "entity_token",
  exact_duplicates = c("report", "drop"),
  existing = c("error", "replace"),
  sensitive_issues = FALSE,
  lock_timeout = 30
)
```

## Candidate Files

- `R/security.R`
- `R/security_linkage.R`
- `R/security_postgres.R`
- `tests/testthat/test-security.R`
- `tests/testthat/test-security-linkage.R`
- `tests/testthat/test-security-postgres.R`
- `vignettes/longitudinal-pseudonymisation.Rmd`
- `vignettes/introduction.Rmd`
- `vignettes/specification-first-eda.Rmd`
- `README.md`
- `NEWS.md`
- `DESCRIPTION`
- `.github/workflows/R-CMD-check.yaml`
- Generated roxygen outputs and `NAMESPACE`
- File-based scaffold README under `inst/`, if present after repository inspection

Exact implementation filenames may be narrowed after inspecting the current security, database inventory, dictionary and catalogue contracts; package behaviour must remain within this specification.

## Risks

- An inferred, normalized or conflicting identity match could join different people or split one person without review.
- Returning database diagnostics verbatim could disclose source identifiers or record-key values.
- Treating repeated identifiers as duplicate records could destroy legitimate longitudinal observations.
- Silently resolving equal record keys with different payloads could corrupt analytic meaning.
- Registry or output partial writes could leave irreversible mismatches unless one transaction owns the full apply operation.
- Concurrent enrolment or destination replacement could create unstable identities or overwrite another run without deterministic locks.
- Replacement authority could broaden into destructive database cleanup unless targets, ownership, object kind and dependencies are checked before a non-cascading change.
- An incomplete or stale dictionary could retain direct identifiers or omit required reviewed columns.
- Documentation could imply anonymity, disclosure control, control over database logging, or portability beyond PostgreSQL/RPostgres.
- Tests, fixtures, messages or examples could accidentally introduce real or project-specific material.

## Authority And Approval

The owner explicitly approved this generic longitudinal PostgreSQL pseudonymisation plan on 2026-08-04 and requested efficient independent sub-agent review. Implementation is authorised on `feature/longitudinal-pseudonymisation`. The approved plan does not authorise a release, tag, force-push, destructive history operation, schema creation, role grant, external database mutation outside disposable test infrastructure, or publication of credentials or restricted data.

## Baseline

Before package-code changes, package-loaded lint was clean. The full test suite recorded 1,573 passing expectations and one existing macOS daylight-saving-time ambiguity failure at `tests/testthat/test-eda-prepare.R:223`. `scripts/check-local.sh` failed from that same existing test. `scripts/check-cran.sh` recorded the same failure plus existing notes for new-submission status, the vignette index, HTTP 403 responses from documented URLs and HTML tidy output. These inherited findings are not acceptance evidence for spec 016 and must not be concealed or weakened.
