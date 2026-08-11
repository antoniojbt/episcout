# Test Design

Spec ID: `034-retained-epi-sec-technical-contract`
Status: Completed

## Design Evidence Completed

- [x] Read all five implementation files containing the seven exports and every transitive internal helper reached from them.
- [x] Inspect current S3 classes/methods, custom conditions, status/severity values, metadata component schemas and PostgreSQL permission queries/mutations.
- [x] Inspect focused unit/live-test definitions, roxygen/generated help, the longitudinal guide, the installed walkthrough and repository callers.
- [x] Compare current `master` with release `0.3.0` to distinguish released from development-only interfaces.
- [x] Run the focused offline pseudonym/linkage/identity-universe baseline; all enabled expectations pass and the live PostgreSQL test skips only because its explicit gate is unset.
- [x] Run the online workflow-state check against canonical GitHub before editing.
- [x] Change no package source, executable test, generated documentation, NAMESPACE, vignette or example in this design issue.

## Cross-slice Invariants

- All seven exports remain present with their retained primary arguments and S3 result classes.
- Independently checked token format, entropy length, uniqueness and collision failure remain unchanged.
- Exact case/whitespace/leading-zero/UUID/integral identity semantics and deterministic collation checks remain unchanged.
- Linkage and universe configuration never contain source identifier values.
- Registry structure/version/constraints and immutable token settings remain unchanged unless a later issue independently justifies a physical migration.
- Null/blank/unmatched/crosswalk-conflict/missing-key/conflicting-payload findings still prevent a requested write.
- Exact duplicates follow only the caller's `report`/`drop` selection; no conflict winner is selected or aggregated.
- Source, registry and output relations remain protected against destructive target collision; replacement remains owned ordinary-table only, dependency-free and non-cascading.
- Advisory locks, deterministic order, lock transfer/cleanup, repeatable-read ownership, inside-transaction revalidation, rollback and row reconciliation retain live regression coverage.
- Authentication/server/driver failures remain sanitised and no test publishes credentials, SQL parameters or identifier values.
- Tests assert that package code neither infers sensitivity nor changes/refuses PostgreSQL grants solely because they include `PUBLIC`.

## First Slice Tests

- Construct version-2 identity-universe specs from exact five-column metadata in both row orders and prove one deterministic fingerprint.
- Accept the exact current six-column input once with one deprecation warning, ignore every `validation_status` value, return the five-column schema and prove the value cannot permit or prevent construction.
- Reject saved or modified version-1 objects with regeneration guidance; reject extra value-bearing fields, too few/duplicate sources, mixed namespaces, unsupported normalisation and empty regex as before.
- Preserve aggregate issue codes/counts and independently derived set expectations for `{A, B, B, C}` and `{B, C, D}` while changing severity to `error`/`warning` and source/namespace status to `error`/`warning`/`ready`.
- Prove audit returns `audit_complete` with and without error-severity issues, always writes nothing and leaks no identifiers.
- Prove materialisation returns `not_written` for invalid inputs, existing destination and lock timeout, and `complete` only after the unique three-column table commits.
- Create a disposable destination schema with explicit `PUBLIC` privileges; prove materialisation is not refused, no grant/revoke SQL is issued and grants are unchanged afterward.
- Preserve ordinary source relation, supported family, deterministic collation, regex, unique constraint, row-count/membership, advisory-lock, statement timeout, transaction ownership and source non-mutation assertions.
- Force an error after destination creation but before commit using a non-permission write boundary; prove complete rollback and a fixed value-free package error.
- Regenerate and inspect both identity-universe Rd files and make only exact interface corrections in the corresponding vignette section.

## Later Slice Tests

- Registry/public-grant tests cover compatible foreign ownership with sufficient server permissions, structural incompatibility, unchanged grants, atomic initialisation, immutable settings and permission-denied sanitisation.
- Linkage tests independently cover exact new schemas, one enrolment source, output-action coverage, identifier-only pseudonymisation, retained record keys, destination uniqueness, crosswalk namespaces and deterministic legacy translation without using privacy/status values.
- Pseudonymisation live tests preserve every current mapping, registry, family, duplicate, destination, lock, rollback and row-reconciliation expectation while migrating status/severity fields.
- Diagnostic tests prove no values are returned unless explicitly requested, requested values print normally, no name/type/value sensitivity inference occurs, the deprecated alias warns once, and no new record-key/payload collection is introduced.
- Manifest tests require `output_type = "pseudonymised_table"` and no `sensitivity` field; output dictionary/catalogue handoff remains accepted by `epi_eda_dictionary_spec()`.

## Design Verification Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'epi_sec_pseudonym|sec-linkage|sec-identity-universe', reporter = 'summary')"
scripts/check-workflow-state.sh
git diff --check
```

## Implementation Verification Commands

Each implementation specification must run its focused unit tests first, the relevant opt-in PostgreSQL tests against an approved disposable database, package lint, `scripts/check-local.sh`, `scripts/check-workflow-state.sh` and `git diff --check`. Run `scripts/check-cran.sh` for the final cross-interface slice or whenever release behaviour is in scope. Render and inspect the longitudinal vignette only when that slice changes it.
