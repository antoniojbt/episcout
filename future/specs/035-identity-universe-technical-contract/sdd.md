# Identity-Universe Technical Contract

Spec ID: `035-identity-universe-technical-contract`
Status: Active

## Authority Boundary

The caller names the source relations, shared namespace, identifier columns, optional PostgreSQL regular expression and destination. Episcout validates the metadata and observed PostgreSQL structure needed for an exact operation, reports aggregate findings and prevents partial or ambiguous writes. PostgreSQL decides whether the connected role may read or write those objects. The package does not confirm, approve, classify or authorise the operation.

## Version-2 Specification

The public arguments and `epi_sec_identity_universe_spec` class remain unchanged. The normal source schema is exactly:

```text
source_schema, source_table, id_column, identity_namespace, provenance
```

The constructor requires at least two unique schema/table pairs, valid PostgreSQL identifiers, one non-empty shared namespace, non-empty provenance, `normalization = "identity"`, and either `NULL` or one non-empty PostgreSQL `validity_regex`. It sorts sources deterministically, sets `contract_version = "identity-universe-2"` and fingerprints the complete contract with SHA-256. It never stores identifier values.

For one development cycle only, the exact same five columns plus `validation_status` are accepted as a legacy construction form. The legacy column is discarded without inspecting or normalising its values, one deprecation warning is emitted per constructor call, and the returned object is an ordinary five-column version-2 object. Missing columns, arbitrary extra columns and value-bearing fields remain errors.

The database boundary accepts only an unmodified version-2 object and verifies its recorded fingerprint by reconstruction. A saved version-1 object, changed field, changed class, changed source shape or changed fingerprint fails with concise guidance to regenerate the specification. No version or fingerprint is inferred or repaired.

## Database Result Contract

All existing public arguments/defaults, result components, aggregate table schemas and materialised columns remain. The completed status rules are:

| Mode/outcome | `status` | `writes` |
| --- | --- | --- |
| Audit completes, with or without findings | `audit_complete` | `FALSE` |
| Materialisation finds any error-severity issue, existing destination or lock timeout | `not_written` | `FALSE` |
| Materialisation commits the new table | `complete` | `TRUE` |

Source and namespace statuses are `ready`, `warning` or `error`. Issue severities are `warning` or `error`. Null, blank, invalid and normalisation-collision findings are errors; repeated identifiers and empty sources remain warnings. Audit is optional technical inspection and never grants permission. Direct materialisation repeats the same checks before and inside its transaction.

Malformed arguments, invalid/saved specifications, caller-owned transactions, unsupported relation/type/collation state, authentication/infrastructure failures and other unsafe database failures remain errors. Native PostgreSQL and driver detail remains behind fixed value-free package messages.

## Set And Write Semantics

The workflow preserves exact typed identity across one compatible text, integral or UUID family; deterministic text collation; optional PostgreSQL regex validation; null, blank, invalid and collision classification; per-source input/observed/distinct/duplicate aggregates; namespace union and membership aggregates; pairwise intersection/exclusive/directional coverage calculations; and `NA` coverage for empty denominators.

Audit remains one `REPEATABLE READ READ ONLY` transaction with a local statement timeout. Materialisation preserves read-only preflight, bounded session advisory lock, transfer to a transaction advisory lock, inside-transaction revalidation, existing-destination refusal, source/destination distinction, new-table-only creation, uniqueness/check constraints, source non-mutation, rollback and post-commit `complete` reporting.

The identity-universe path performs no schema/table privilege query and executes no `GRANT` or `REVOKE`. An output schema with explicit `PUBLIC` privileges is treated like any other schema: materialisation succeeds only when the connected role has the PostgreSQL permissions needed for the SQL, and grant state is unchanged by episcout.

## Ordinary Output Boundary

Ordinary results remain aggregate and value-free. `source_audit`, `namespace_audit`, `overlap_audit` and `issues` contain relation metadata, counts, rates, codes and technical text only. No source identifier, canonical identifier, sample value or value-bearing diagnostic is added. The materialised table remains the only requested value-bearing output.

## Compatibility

The exports are absent from release `0.3.0`. Successful current `audit_complete` and `complete` consumers remain compatible except that contract metadata is version 2. Current blocked-audit consumers must inspect `issues$severity == "error"`; materialisation no-write consumers must use `not_written`. Saved version-1 objects must be regenerated. The legacy constructor adapter is syntax-only and a `validation_status` value can neither permit nor prevent construction or execution.

## Deferred Boundary

This slice does not modify shared `sec_schema_is_public()` or privilege behaviour used by the registry, linkage or main pseudonymisation paths. The second tracker may be created only after version-2 identity-universe merge and closeout pass the workflow-state check.
