# Neutral Linkage, Result, Issue And Diagnostic Contract

Spec ID: `037-epi-sec-linkage-results-neutral`
Status: Active

## Authority Boundary

The caller declares source relations, identity namespaces, enrolment source, row grain, output destinations, output actions, record keys, crosswalks, duplicate handling, replacement behaviour and whether ordinary identifier-value diagnostics are returned. episcout validates that the declared operation is structurally unambiguous and can preserve exact mapping, row grain and database integrity. The package does not confirm the request, classify columns or values, infer sensitivity, decide whether an output may be used or disclosed, or hide a caller-requested ordinary value based on package policy.

## Linkage Metadata Contract

The four component names, export names, function arguments and S3 classes remain. The exact version-2 component schemas are:

```text
tables:
source_schema, source_table, id_column, identity_namespace,
can_enrol, one_row_per_entity, destination_table, provenance

columns:
source_schema, source_table, source_column, output_action

record_keys:
source_schema, source_table, key_column, key_order

crosswalks:
crosswalk_schema, crosswalk_table, alias_namespace, alias_id_column,
canonical_namespace, canonical_id_column, provenance
```

`epi_sec_linkage_scaffold()` returns only these schemas and initialises every `output_action` to blank so it makes no projection choice. `epi_sec_linkage_spec()` accepts exactly `pseudonymise`, `retain` or `drop`. Every declared dictionary column has one action, exactly the declared `id_column` is `pseudonymise`, no other column is `pseudonymise`, and every record-key column is `retain`. Existing source/table uniqueness, one enrolment source, row-grain, destination, namespace, crosswalk and metadata-only checks remain.

For one development cycle, each exact current-master legacy component schema is accepted as syntax only. The constructor ignores `validation_status` and `privacy_class` regardless of value, maps `bridge` to `pseudonymise`, `retain` and `retain_restricted` to `retain`, and `drop` to `drop`, emits one migration warning per constructor call, and always returns the new schemas. `review`, `derive`, arbitrary actions, extra columns and value-bearing fields remain errors. Legacy confirmation or privacy values cannot permit, prevent or otherwise change construction. Saved legacy linkage objects are rejected at the database boundary with regeneration guidance.

## Pseudonymisation Result Contract

The public arguments/defaults and `epi_sec_pseudonymisation_result` class remain except for the intentional diagnostic extension. `include_issue_values = FALSE` is added after the existing positional arguments. `sensitive_issues = NULL` remains temporarily in its existing positional slot as a deprecated alias; using it emits one warning, maps only to `include_issue_values`, and errors if it conflicts with an explicitly supplied `include_issue_values` value.

Completed result states are:

| Mode and outcome | `status` | `writes` |
| --- | --- | --- |
| Audit completes with or without issues | `audit_complete` | `FALSE` |
| Apply preflight or protected transaction finds an error-severity issue or lock timeout | `not_written` | `FALSE` |
| Apply commits registry and output changes | `complete` | `TRUE` |

Authentication, permission, driver and PostgreSQL failures remain sanitised errors rather than results. Malformed arguments, unsupported types, invalid component schemas and structural state that cannot form a complete audit remain precise errors. Runtime dictionary/action/catalogue coverage failures no longer use an `epi_sec_governance` carrier and instead use ordinary technical validation errors.

The issue table contains exactly `issue_code`, `severity`, `stage`, `source_schema`, `source_table`, `source_column`, `n_affected`, `message` and `recommended_action`. Severity is `error` or `warning`; an error means the requested apply cannot produce the declared technical result. Internal protected-transaction no-write flow uses `epi_sec_no_write`, which is caught before return and is not exposed to successful callers. `blocked`, `blocking`, `epi_sec_blocked`, `epi_sec_governance` and governance-specific result builders are removed.

## Diagnostic Values

When `include_issue_values = FALSE`, results contain only aggregate/value-free issues. When `TRUE`, `issue_values` is an ordinary data frame with `issue_code`, source relation/column metadata and `source_value` for the existing `invalid_identifier` and `unmatched_identifier` families only. It has no custom class, sensitivity attribute or specialised print/structure method. The result summary remains aggregate, but directly printing or inspecting `issue_values` behaves exactly like any ordinary data frame.

If the deprecated alias is explicitly `TRUE`, a temporary result component named `sensitive_issues` points to the same ordinary diagnostic rows so existing extraction does not silently lose requested values. No alias component is returned for `FALSE` or when the alias is unused. The implementation does not begin collecting record-key payloads, crosswalk rows or arbitrary source values.

## Projection And Manifest Contract

Pseudonymisation consumes only `output_action`: the declared identifier becomes the generated token, `retain` columns are copied and `drop` columns are omitted. The output dictionary/catalogue handoff and token metadata remain unchanged. The manifest contains `source_schema`, `source_table`, `output_schema`, `output_table`, `status` and `output_type`; `output_type` is always `pseudonymised_table` and the former inferred `sensitivity` field is removed.

## Preserved Technical Behaviour

- Exact case, whitespace, leading-zero, integral and UUID identity semantics and deterministic collation validation.
- Stable registry enrolment, immutable aliases, token generation/collision handling and configuration hashing.
- Crosswalk relation/type/namespace/target/conflict/chain/blank/registry-conflict checks.
- Exact duplicate `report`/`drop`, record-key missing/conflict detection and no arbitrary winner or aggregation.
- Source/registry/output separation, destination collision and ownership-safe dependency-free non-cascading replacement.
- Repeatable-read validation, advisory-lock acquisition/transfer/release, transaction ownership, rollback, source non-mutation and row reconciliation.
- Fixed value-free PostgreSQL authentication, permission and infrastructure failures.

## Compatibility

Release `0.3.0` includes the linkage and pseudonymisation exports, but the current four-component linkage schema is unreleased and already has a migration boundary. Successful `audit_complete` and `complete` consumers, result classes, aggregate audit schemas, output dictionaries/catalogues, output tables and database effects remain stable. Consumers of confirmation/privacy fields, `blocked`, `blocking`, issue/manifest sensitivity, redacting diagnostic classes or saved legacy linkage objects must migrate to the explicit neutral contract. The adapter is bounded to deterministic construction syntax and never preserves policy meaning.

## Documentation Boundary

Roxygen/Rd, the installed walkthrough, the standalone pseudonym helper cross-reference and only the directly affected linkage/result snippets in the longitudinal guide are updated with the implementation. `issue-269` retains authority for the later complete operator-journey rewrite and reconciliation of remaining governance-oriented prose.
