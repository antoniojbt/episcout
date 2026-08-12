# Longitudinal Pseudonymisation Documentation Contract

Spec ID: `038-longitudinal-pseudonymisation-documentation`
Status: Review

## Authority Boundary

Callers select source relations, identifier namespaces, validity expressions, enrolment source, output actions, row grain, record keys, database-resident crosswalks, registry and output schemas, duplicate handling, destination replacement and optional identifier-value diagnostics. episcout validates declared metadata and current PostgreSQL state, performs exact technical operations and returns observed technical results. It does not classify whether processing, use, sharing or publication is permitted and does not manage credentials, database permissions, backups, logging or other operational controls.

## Public Interface Evidence

The guide covers all seven retained exports and their current formals:

```text
epi_sec_pseudonym(participant_id, n_bytes, prefix, bridge_path, overwrite)
epi_sec_identity_universe_spec(sources, normalization, validity_regex)
epi_sec_identity_universe_db(con, spec, mode, output_schema, output_table, existing, statement_timeout, lock_timeout)
epi_sec_linkage_scaffold(dictionary, tables)
epi_sec_linkage_spec(tables, columns, record_keys, crosswalks)
epi_sec_identity_registry_init(con, registry_schema, token_prefix, n_bytes, mode)
epi_sec_pseudonymise_db(con, dictionary, linkage, registry_schema, output_schema, catalogues, mode, token_column, exact_duplicates, existing, sensitive_issues, lock_timeout, include_issue_values)
```

The public S3 objects and their schemas are established by the current source and focused tests:

- `epi_sec_identity_universe_spec`: `sources`, `normalization`, `validity_regex`, `contract_version`, `fingerprint_sha256`.
- `epi_sec_identity_universe_result`: `status`, `metadata`, `source_audit`, `namespace_audit`, `overlap_audit`, `issues`.
- `epi_sec_linkage_scaffold` and `epi_sec_linkage_spec`: `tables`, `columns`, `record_keys`, `crosswalks` using the exact neutral component schemas documented by the roxygen source.
- `epi_sec_registry_result`: `status`, `mode`, `writes`, `registry_schema`, `metadata`, `objects`, `next_action`.
- `epi_sec_pseudonymisation_result`: `status`, `metadata`, `identity_audit`, `table_audit`, `duplicate_audit`, `issues`, `output_dictionary`, `output_catalogues`, `manifest`, plus ordinary caller-selected diagnostic components only when requested.

## Technical Operation And Status Semantics

Identity-universe audit owns a read-only repeatable-read transaction and returns `audit_complete` after a completed inspection regardless of warning/error findings. Materialisation repeats validation in a protected transaction, writes a new three-column universe only after error-free checks, returns `not_written` for error findings, destination existence or lock timeout, and returns `complete` only after commit.

Registry audit is the default and reports `initialisation_required`, `incompatible` or `ready` without writing. Registry apply requires an idle caller connection, creates all six physical registry tables in one repeatable-read transaction when the schema is empty, returns `ready`, and writes nothing when the compatible registry already exists. Token settings are immutable for that registry. Structure and version determine compatibility; episcout neither queries nor changes privileges.

Pseudonymisation audit is optional, writes nothing and returns `audit_complete` even when issues are present. Apply repeats all validation, obtains bounded session advisory locks before transferring protection to deterministic transaction-scoped locks, owns one repeatable-read transaction, and commits registry aliases, outputs and aggregate run metadata together. Error-severity findings and lock timeout return `not_written`; unexpected apply failures return a sanitised error after rollback. Source tables are never changed.

## Exact Identity, Duplicate And Stable-Token Semantics

Text matching preserves case, leading zeros and nonblank whitespace under deterministic byte-distinguishing comparisons; integral and UUID families use their PostgreSQL identity semantics. Fixed-width identifiers, unsupported types and nondeterministic text collations are rejected. Crosswalks are exact database-resident mappings; chains, cycles, conflicting targets, missing targets, invalid values and conflicts with immutable registry aliases are errors.

The registry provides stable tokens only for the same stored assignment: the registry, identity namespace and any alias/crosswalk mapping must resolve an identifier to the same entity token. Its aliases contain plaintext source identifiers and make the mapping reversible. `epi_sec_pseudonym()` is a separate one-vector random bridge helper and does not provide stability across calls.

`exact_duplicates = "report"` retains every exactly repeated projected row and reports excess counts; `"drop"` removes only redundant identical projections. Equal declared token/key values with different retained payloads are error findings and no row is selected, aggregated or overwritten. A table with no one-row declaration or record key can assess only exact projected duplicates and receives a warning.

## Replacement And Recovery

`existing = "error"` refuses occupied pseudonymisation destinations. `existing = "replace"` is a caller-selected destructive operation limited to declared, dependency-free, non-partition ordinary tables owned by the connected role and uses no `CASCADE`; it is not an approval mechanism. Identity-universe materialisation supports only `existing = "error"`.

After `not_written`, callers inspect issues and observed database state, correct the technical cause and retry audit or apply. After a sanitised error, callers confirm the transaction outcome through PostgreSQL state/logs available to them, correct the infrastructure or database cause and retry from an idle connection. Incompatible registries require a separate recovery operation rather than manual row edits.

## EDA Handoff And External Responsibilities

Pseudonymisation returns a dictionary for actual outputs, retained referenced catalogues and a manifest whose `output_type` is `pseudonymised_table`. Those objects can be passed to `epi_eda_dictionary_spec()`. This technical handoff does not make the data anonymous, automatically disclosure-controlled or permitted for use, sharing or publication. Credential management, PostgreSQL privileges, storage, backups, retention, logging and decisions about downstream use remain outside episcout.

## Documentation Changes

Replace governance, approval, mandatory review/audit and prescribed restricted-schema wording with observable behaviour and explicit caller choice. Keep neutral synthetic/disposable examples and warnings against exposing identifiers, tokens, credentials or database details. Update roxygen and regenerate Rd only if comparison against this verified contract finds a mismatch; otherwise leave generated help unchanged.

## Compatibility

No public formal, class, component name, status, error, database object or side effect changes. The documentation correction may change readers' interpretation from package-controlled permission to caller-selected technical operation, which is the intended compatibility correction established by completed `spec-034` through `spec-037`.
