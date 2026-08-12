# Registry And PostgreSQL Privilege-Neutrality Contract

Spec ID: `036-epi-sec-registry-neutral`
Status: Review

## Authority Boundary

The caller names the existing source, registry, crosswalk and output schemas and requests audit or apply behaviour. episcout validates arguments, relation structure, registry compatibility, exact linkage invariants and write safety. PostgreSQL decides whether the connected role may inspect or mutate the requested objects. The package does not query, interpret, grant or revoke privileges and does not require registry-table ownership when PostgreSQL otherwise permits the operation.

## Registry Result Contract

The public arguments, defaults, `epi_sec_registry_result` class, registry metadata columns, object table and physical registry version remain unchanged. The result contains exactly `status`, `mode`, `writes`, `registry_schema`, `metadata`, `objects` and `next_action`; `schema_restricted` is removed.

The completed result states are:

| Registry outcome | `status` | `writes` |
| --- | --- | --- |
| Audit finds no registry objects | `initialisation_required` | `FALSE` |
| Audit finds a complete structurally compatible version-1 registry | `ready` | `FALSE` |
| Apply commits a new compatible version-1 registry | `ready` | `TRUE` |
| Audit finds missing, wrong-kind, structurally incompatible or wrong-version registry objects | `incompatible` | `FALSE` |

Apply against an incompatible or concurrently changed registry remains an error and changes no object. A compatible registry with a different token prefix or byte count retains the immutable-settings error. The result printer reports status, schema, writes and next action without a privilege classification.

Object statuses remain structural: `planned`, `absent`, `present`, `wrong_kind` and `incompatible_structure`. Registry inspection must not select table owners or privilege predicates and must not classify `foreign_owner` or `public_access`.

## Registry And Pseudonymisation SQL Boundary

Registry creation preserves the six version-1 tables, columns, collations, defaults, constraints and single metadata row. It remains one repeatable-read transaction and is re-inspected after commit. No registry creation statement grants or revokes schema/table privileges.

The stable-registry path in `epi_sec_pseudonymise_db()` preserves source/registry/output schema separation, registry reinspection before and inside the transaction, crosswalk ordinary-table/type/collation validation, advisory locks, registry reconciliation, atomic output creation, exact duplicate behaviour and post-write row reconciliation. It removes only:

- registry/output schema `PUBLIC` privilege checks before and inside apply;
- registry table owner and `PUBLIC` access checks;
- crosswalk schema/table `PUBLIC` checks;
- registry and output table/schema `REVOKE` statements; and
- now-unused privilege helper functions.

`existing = "replace"` remains limited to an ordinary, non-partitioned output table owned by the connected role and without external dependencies. This is destructive-target safety, not a general privilege policy. Source, registry and output relations remain distinct and source tables are never mutated.

## Failure Contract

Malformed arguments and structural incompatibility retain precise fixed errors. Authentication, permission, driver and PostgreSQL failures are ordinary server outcomes caught by the existing database boundary and returned as fixed value-free package conditions. No native SQL, identifier value, credential or restricted row is added to an ordinary result or error.

## Compatibility

The exports are absent from release `0.3.0`. Current development consumers must remove `schema_restricted` reads and replace registry `status == "blocked"` checks with `status == "incompatible"`. Registry structure/version, token settings, result class and successful `initialisation_required`/`ready` states remain compatible. Existing database grants are preserved exactly rather than tightened by package code.

## Deferred Boundary

This slice does not rename pseudonymisation `blocked` statuses, `blocking` severities, governance conditions, linkage confirmation/privacy fields, diagnostic classes or manifest sensitivity. Those changes remain exclusively assigned to `issue-285`, followed by the separately gated documentation reconciliation.
