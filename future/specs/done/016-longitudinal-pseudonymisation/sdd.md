# Software Design

Spec ID: `016-longitudinal-pseudonymisation`
Status: Implemented

## Semantic And Privacy Authority

The caller-supplied, current, complete and confirmed multi-table dictionary is authoritative for column privacy class, analytic action, semantic role, catalogue reference and output eligibility. The linkage specification is authoritative for exact identity namespace, enrolment authority, destination relation, row grain, record keys, crosswalk relation metadata and provenance. Observed database values determine whether those reviewed contracts are executable; they never establish identity equivalence, privacy class, row grain, keys, provenance or approval.

Identity matching is exact and deliberately reviewed. The implementation performs no trimming, case-folding, hashing for comparison, phonetic processing, similarity matching, inference or probabilistic linkage. Pseudonymised data remain restricted personal data. The workflow does not make data anonymous, disclosure-controlled or safe to publish.

## Backend Boundary

PostgreSQL through `RPostgres` is the only persistent backend supported by this specification. Validate the connection and backend before database inspection. Source, registry and output schemas must be distinct, pre-existing and appropriately restricted. The workflow does not create schemas, grant roles, manage backups, configure server/driver/administrator logs or claim that identifiers cannot enter infrastructure outside package control.

Source relations and crosswalks must be ordinary PostgreSQL tables. Database views and materialized views are out of scope. The workflow must quote every identifier with DBI facilities and parameterise values; never interpolate caller values or observed identifiers into SQL text.

## Linkage Scaffold

```r
epi_sec_linkage_scaffold(dictionary, tables = NULL)
```

The function inspects dictionary metadata only and never reads database row values. `dictionary` must use the package's reviewed multi-table dictionary contract. `tables` is `NULL` or a value-free selection/override table accepted by the documented scaffold contract.

Return a plain list of draft `tables`, `record_keys` and `crosswalks` data frames. The tables draft uses exactly:

```text
source_schema
source_table
id_column
identity_namespace
can_enrol
one_row_per_entity
destination_table
provenance
validation_status
```

The scaffold may copy source relation names and candidate direct-identifier/bridge column names already present in the dictionary. It must not infer identity namespaces, enrolment permission, one-row-per-entity semantics, destination names, provenance, keys or confirmation. These fields remain explicitly missing or `unreviewed`. Record-key and crosswalk outputs have stable zero-row schemas until reviewed metadata are supplied. Printing explains exactly which fields require human review and does not show identifier values.

## Reviewed Linkage Specification

```r
epi_sec_linkage_spec(tables, record_keys = NULL, crosswalks = NULL)
```

`tables` has the exact columns listed above. Require non-empty, valid PostgreSQL identifier names for schema/table/ID/namespace/destination fields, unique `(source_schema, source_table)` selections, unique destination names, non-missing scalar logical values, non-empty provenance and `validation_status = "confirmed"` for every row. Require exactly one `can_enrol = TRUE` row. Do not require the enrolment table to have one row per entity.

`record_keys` has exactly:

```text
source_schema
source_table
key_column
key_order
```

Every row refers to one selected table. Keys have positive consecutive integer order within a table and no duplicate key columns. A table declared `one_row_per_entity = TRUE` uses the generated token alone as its record key and therefore must not also declare key columns. A table not declared one-row-per-entity may have an ordered composite key or no declared key; the latter is allowed but produces an explicit audit warning about the inability to identify conflicting repeated observations.

`crosswalks` contains metadata only, never alias or canonical values. Its exact columns are:

```text
crosswalk_schema
crosswalk_table
alias_namespace
canonical_namespace
alias_id_column
canonical_id_column
provenance
validation_status
```

Every crosswalk row has non-empty relation/column/namespace/provenance metadata and confirmed status. Namespace references must be declared by selected tables. A same-namespace crosswalk is permitted for explicitly reviewed deduplication aliases. The canonical side must ultimately resolve to the single enrolment namespace. Reject duplicate metadata rows and statically detectable namespace cycles. Database-dependent missing targets, conflicting mappings and unused rows are audited inside PostgreSQL.

Return class `c("epi_sec_linkage_spec", "list")` with exact `tables`, `record_keys` and `crosswalks` components and normalized deterministic row order. Its print method shows table counts, the enrolment relation, how many tables have declared keys, crosswalk metadata count and the next action. It never prints identifier values or key values.

## Identity Registry Initialisation

```r
epi_sec_identity_registry_init(
  con,
  registry_schema,
  token_prefix = "E",
  n_bytes = 24,
  mode = c("audit", "apply")
)
```

Validate one live RPostgres connection, one non-empty existing `registry_schema`, a conservative printable token prefix, integer `n_bytes` within a documented security-preserving range and mode through `match.arg()`. The registry schema represents one deliberately authorised linkage domain. Generate an immutable random `registry_id`; accept no project name, study name, institution or project identifier.

Audit mode writes nothing. It checks schema existence, caller access, denial of schema `CREATE` to `PUBLIC`, expected object presence/absence, ordinary-table kind, compatible columns/constraints/version and absence of unsafe partial/incompatible state. It returns a redacted status with actionable issues.

Apply mode creates or validates, in one transaction, versioned tables:

- `registry_metadata`: one immutable registry identity, schema version, token parameters and value-free timestamps.
- `namespaces`: unique namespace labels and immutable semantic type family.
- `entities`: surrogate entity identity and globally unique random token.
- `aliases`: unique `(namespace, typed identity)` resolution to exactly one entity.
- `runs`: successful value-free run metadata, configuration hashes and timestamps.
- `run_tables`: successful aggregate per-table counts and provenance linked to a run.

The storage representation must preserve exact text identity, integral equality and PostgreSQL UUID identity without accidental coercion between families. Constraints and foreign keys enforce registry consistency. Token material is generated cryptographically in R using an existing dependency, consists of `token_prefix` plus an encoded random payload, and is independent of source identifier values. Retry token creation within a small fixed bound on the extraordinarily unlikely unique collision and fail atomically if exhausted.

After table creation, revoke all table privileges from `PUBLIC`. Require the schema itself to deny `PUBLIC` creation before any apply. Role-specific grants remain a database-administrator responsibility. Existing compatible registries retain their immutable registry ID and configuration; incompatible versions, object kinds, ownership or token configuration are errors or blockers according to the documented unsafe-state boundary.

## Main Workflow

```r
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

Validate all top-level arguments before database writes. `linkage` must be a confirmed `epi_sec_linkage_spec`. `registry_schema`, `output_schema` and every source/crosswalk schema must exist and registry/output/source roles must be distinct. `token_column` must be one non-empty PostgreSQL column identifier and cannot collide with a retained source column. `sensitive_issues` is scalar non-missing logical. `lock_timeout` is one finite non-negative number of seconds within a documented safe bound.

The workflow returns class `c("epi_sec_pseudonymisation_result", "list")` with fixed components in this order:

```text
status
metadata
identity_audit
table_audit
duplicate_audit
issues
output_dictionary
output_catalogues
manifest
```

When and only when `sensitive_issues = TRUE`, append a separately named `sensitive_issues` memory-only component marked sensitive. It is excluded from printing, persistent registry audit rows, manifests and configuration hashes. Documentation warns users that printing, logging or retaining that component is their responsibility.

`status` is exactly `audit_complete`, `blocked` or `complete`. Audit mode cannot return `complete`. Expected governance/data findings return `blocked`; malformed arguments, unsupported identifier/key types, caller-managed transactions, incompatible database objects, unsafe privileges/state and infrastructure failures are errors unless the contract names a sanitized blocked result. `metadata` states mode, whether writes occurred, registry/output schemas, stable contract/version, requested policies and aggregate run timing without source values or local paths.

The print method shows status, mode, whether writes occurred, aggregate entity/table/duplicate counts, output schema and a concise next action. It never shows identifiers, record keys, tokens, native database detail or sensitive-component contents.

## Fixed Issue Contract

Every ordinary issue table has exactly:

```text
issue_code
severity
stage
source_schema
source_table
source_column
n_affected
message
recommended_action
sensitive
```

Rows use deterministic stage/table/code order. `severity` is `info`, `warning` or `blocking`; ordinary rows always have `sensitive = FALSE`. Relation/column names may appear because they are reviewed metadata, but observed identifiers, record-key values, tokens, native SQL detail and free text never appear. `n_affected` is an aggregate count or typed missing value. Known database conditions are caught and converted into sanitized package conditions/results; native messages must not be embedded in public errors or issues when they could contain data values.

## Dictionary Gate And Output Projection

Before identity work, inventory every selected ordinary source relation in PostgreSQL and reconcile it with the supplied dictionary. Require complete, current and confirmed coverage for every selected source column. Block unclassified or pending rows, added/modified drift, missing dictionary/source columns, unsupported `derive`, incompatible privacy/action pairs, stale catalogues and token-column collisions.

Every selected ID column must be classified as a direct identifier with analytic action exactly `bridge`. Remove all `bridge` and `drop` columns from output. Retain only confirmed `retain` and `retain_restricted` columns, preserving source type and column order except that the selected ID position is replaced by the generated token column. Do not permit any other action to pass silently.

Describe the generated token in the output dictionary with actual destination schema/table/column metadata, `role = "id"`, `privacy_class = "sensitive"`, `analytic_action = "retain_restricted"`, confirmed review status and generated provenance. Preserve and validate referenced catalogue definitions through `catalogues`; return only definitions referenced by retained output columns. The resulting dictionary/catalogues must validate through current package contracts and hand directly into `epi_eda_dictionary_spec()`.

## Identifier Families And Exact Matching

Support PostgreSQL text-like, integral and UUID identifier families only. Inventory the declared ID and crosswalk column PostgreSQL types, map them to one semantic family and store that family on first namespace use. Refuse incompatible reuse of a namespace.

- Text identity preserves case, whitespace and leading zeros exactly. Null, empty and whitespace-only values are rejected, but non-empty surrounding whitespace remains significant and is not trimmed.
- Integral identity uses exact PostgreSQL integral equality and no floating-point or lossy R conversion.
- UUID identity uses PostgreSQL UUID equality; lexical formatting differences accepted by PostgreSQL are therefore the same identity.

Do not retrieve source identifiers merely to normalize or match them in R. Generate tokens in R without source values, stage unpaired tokens in `pg_temp`, and associate tokens to newly enrolled source identifiers inside PostgreSQL.

The single enrolment table may create registry entities for exact identifiers not yet present in its namespace. Repeated enrolment identifiers resolve to the same entity and do not imply duplicate records. Dependent-table identifiers must resolve through an existing same-namespace alias or confirmed crosswalk path to an enrolment identity present in this run or an existing registry alias.

Allow many reviewed aliases to resolve to one entity. Block conflicting alias assignments, alias or namespace cycles, duplicate crosswalk aliases with different canonical targets, missing canonical targets and unmatched dependent identifiers. Report unused crosswalk rows only by aggregate count. Never infer that similar values or similar retained records represent the same entity.

## Longitudinal Records And Duplicates

Identity deduplication and row deduplication are separate operations. Repeated entity identifiers are valid in every table.

For `one_row_per_entity = TRUE`, the generated token alone is the declared record key. Otherwise, ordered key columns produce `(entity_token, key columns)`. Require every key component to be retained in the final projection, non-missing and backed by a PostgreSQL type with reliable equality. Reject unsupported equality types before output writes.

An equal record key with payload values that are not all PostgreSQL `IS NOT DISTINCT FROM` equivalent is a conflict and blocks the run. Never select a winner, prioritize a row, aggregate values, average, or silently collapse a conflict.

An exact projected duplicate is a row whose entire final pseudonymised projection is not-distinct from another row. `exact_duplicates = "report"` preserves all rows and reports aggregate duplicate counts. `exact_duplicates = "drop"` deliberately removes redundant projected copies and reports the number removed. With no declared key, inspect exact projected duplicates only and warn that conflicting repeated observations cannot be identified.

For every selected table reconcile source row count, successfully identity-linked row count, projected input count, output row count and explicitly removed projected duplicates. No unexplained row gain/loss may complete.

## Audit Mode

Audit mode performs all safe read-only preflight possible: backend/schema/privilege/object checks, registry compatibility, source inventory and drift, dictionary/catalogue gates, destination collision/ownership/dependency checks, identifier-family compatibility, missing/blank/unmatched identity counts, crosswalk consistency and usage counts, record-key validity, projected duplicates/conflicts and row reconciliation. It creates no registry entity, alias, run record, output table, persistent staging table or schema object.

Return `blocked` when any blocker exists and `audit_complete` otherwise. An audit-complete result is permission to consider apply, not evidence that a later concurrent database snapshot will pass. Issues give an exact next action.

## Atomic Apply And Concurrency

Reject invocation while the connection is already inside a caller-managed transaction because the workflow must own the full boundary. Start one `REPEATABLE READ` transaction using DBI's rollback-on-error contract. Re-run all inventory, drift, privilege, destination, identity, duplicate and reconciliation checks after the transaction begins; do not rely on a prior audit result.

Acquire transaction-scoped PostgreSQL advisory locks in deterministic registry-then-destination order. Derive lock keys from stable registry/destination metadata without source identifier values. Apply the caller's bounded `lock_timeout`; lock timeout returns a sanitized value-free `blocked` result with no writes rather than waiting indefinitely.

Use `pg_temp` staging objects with `ON COMMIT DROP`. Only after all blockers pass may the transaction enrol identities, add aliases, create/replace output relations and persist successful aggregate audit metadata. A failure at any write phase rolls back every registry and output change.

With `existing = "error"`, any destination object blocks before mutation. With `existing = "replace"`, replace only the exact declared ordinary destination table when it is owned by the current caller and has no dependency that prevents a plain non-cascading drop. Never use `CASCADE`. A view, foreign-owned object, unexpected relation kind, dependency or ownership conflict blocks and rolls back.

Output relations preserve retained source column PostgreSQL types and order while replacing the source ID column with the configured token column. Source rows, relation structure, constraints, comments, ownership and grants remain unchanged. Registry/output privileges remain restricted; never broaden public access as a convenience.

Persist only successful, value-free run metadata, normalized configuration hashes, confirmed provenance and aggregate per-table counts. Audit, lock-timeout and blocked modes persist nothing. The manifest and hashes exclude identifiers, record keys, tokens and the optional sensitive component.

## Manifest And Audits

`identity_audit`, `table_audit` and `duplicate_audit` use documented stable aggregate schemas and deterministic order. They expose counts and statuses sufficient to reconcile enrolment, reuse, aliases, matching, source/projected/output rows and duplicate decisions without values.

`manifest` is a stable value-free data frame that identifies each source and destination relation, planned or created status and restricted-data sensitivity. Aggregate counts remain in `table_audit`; successful configuration hashes and run metadata remain in the restricted registry. Audit mode marks every database output as planned. Apply results mark only successfully committed outputs as created. The manifest contains no native SQL, source values, tokens, key values, credentials, connection strings or local paths.

## Documentation And Discovery

Add `vignettes/longitudinal-pseudonymisation.Rmd` as the canonical guide. Keep database chunks unevaluated during ordinary builds and use only neutral schemas `source_data`, `identity_registry`, `analysis_data`; neutral tables `entities`, `events`, `measurements`; and obviously synthetic opaque identifiers generated at runtime. Never include credentials or connection strings.

Make the guide discoverable in one step from README Features and Getting Started, package-level help, introductory and specification-first vignettes, database inventory/dictionary help, and every pseudonymisation help page through a shared roxygen family and reciprocal `@seealso` links. Add a short “Sensitive database sources” pointer in the file-based scaffold README warning users not to export identifiable database rows merely to use the EDA scaffold.

The guide order is:

1. Purpose and appropriate use.
2. DBA, schema, privilege, backup and access prerequisites.
3. Neutral synthetic enrolment and event relations.
4. Inventory and dictionary review.
5. Linkage scaffold and confirmed specification.
6. Optional restricted database-resident crosswalk.
7. Registry audit and initialization.
8. Workflow audit before data writes.
9. Statuses, blockers and exact next actions.
10. Apply, output verification and stable-token rerun.
11. Duplicate-behaviour decision table.
12. Output dictionary handoff to EDA.
13. Replacement, rollback, concurrency and recovery.
14. Privacy, access control, backup, logging and disclosure limitations.
15. Troubleshooting.

Every new help page documents accepted columns/types, defaults, status meanings, return schemas, no-write guarantees, errors versus blockers, privacy limitations and recovery. NEWS receives a concise outcome summary. Documentation states prominently and repeatedly that pseudonymised data remain restricted personal data and are not anonymous or automatically disclosure-controlled.

## Compatibility And Dependencies

The change is additive. Preserve `epi_sec_pseudonym()` formals, defaults, classes, values, error behaviour and released examples except for replacing project-like sample material and adding reciprocal discovery links. Reuse existing dependencies where adequate. Add `RPostgres` as an optional test/runtime dependency only if repository inspection confirms it is not already declared; the exported PostgreSQL APIs must fail actionably when the optional backend is unavailable.

## Genericity And Repository Safety

No implementation, fixture, default, example, test, message or documentation added or changed by this spec may contain a real study, institution, geography, disease programme, person, credential, developer path, source-system name or other project-specific material. Use hand-authored neutral structures and runtime-generated opaque identifiers. Audit all touched repository content before handoff. Do not use existing real-package datasets as fixtures for this feature.

## Recovery

Implementation was completed on `feature/longitudinal-pseudonymisation` and merged through PR #189. PostgreSQL apply is recovered by automatic transaction rollback; a blocked audit/apply writes nothing. If exact identity semantics, source non-mutation, restricted privileges, atomic replacement, value-free diagnostics or dictionary gates cannot satisfy this contract, stop and record the conflict in `review.md` rather than weakening the privacy or integrity boundary. No release or tag was created.
