# Retained `epi_sec` Technical Contract

Spec ID: `034-retained-epi-sec-technical-contract`
Status: Active design inventory

## Authority Boundary

The caller chooses source relations, identifier namespaces, output columns, transformation actions, duplicate handling, destinations, diagnostic values and whether to inspect or write. PostgreSQL decides whether the connected role may read or mutate the named objects. Episcout validates the portable metadata and observed database structure required to execute that request exactly, prevents partial or ambiguous writes, and reports what happened. It does not approve the request, infer personal-information status, decide whether access is appropriate, or determine whether an output may be shared or published.

## Classification Key

| Class | Meaning in this design | Disposition |
| --- | --- | --- |
| 1. Statistical/structural correctness | A rule needed to define one unambiguous identifier universe, linkage, row grain, projection or output schema. | Retain, using neutral validation language. |
| 2. Database/transaction/data-integrity correctness | A rule needed to avoid partial writes, wrong-object mutation, identity drift, token collision, inconsistent registry state or uncontrolled native database detail. | Retain and test at the same or stronger level. |
| 3. Explicit user-selected transformation or diagnostic behaviour | A caller choice such as audit/apply, retain/drop/pseudonymise, duplicate handling, replacement or identifier-value diagnostics. | Retain as an explicit option without treating it as approval. |
| 4. Package-imposed governance/privacy/disclosure policy | A package decision based on confirmation, privacy classification, schema restriction, `PUBLIC` privileges, sensitivity labels, redaction or permission language. | Remove, replace with a caller choice, or convert to a precise class 1/2 validation. |

## Public Interface Impact

| Export | Current arguments and result | Intended contract | Compatibility and migration |
| --- | --- | --- | --- |
| `epi_sec_pseudonym()` | `participant_id`, `n_bytes = 24`, `prefix = "P"`, `bridge_path = NULL`, `overwrite = FALSE`; returns a two-column tibble. | Retain all arguments, defaults, output columns, input-order preservation, cryptographic random generation, minimum 16 bytes, collision retry/failure, optional CSV write and explicit overwrite choice. Update only policy-oriented cross-reference prose. | Fully compatible. This is a released `0.3.0` interface and repository tests cover its complete behavioural surface. |
| `epi_sec_linkage_scaffold()` | `dictionary`, optional `tables`; returns `tables`, `columns`, `record_keys`, `crosswalks` with review/confirmation and privacy fields. | Retain arguments, class and four component names. Remove `validation_status` from `tables`/`crosswalks`; replace `columns` fields `privacy_class`, `analytic_action`, `validation_status` with one `output_action`; initialise `output_action` to blank so the scaffold makes no retention decision. Keep metadata-only operation and active-table selection. | Result-column break is deliberate. Current-master legacy components receive a one-cycle input adapter in `epi_sec_linkage_spec()`; the scaffold itself returns only the new schema. The older released three-component scaffold already has current migration guidance and cannot be upgraded without a caller-supplied column selection. |
| `epi_sec_linkage_spec()` | `tables`, `columns = NULL`, optional `record_keys`, optional `crosswalks`; requires `confirmed` statuses and privacy/action combinations. | Retain function arguments and four-component class. `tables` contains `source_schema`, `source_table`, `id_column`, `identity_namespace`, `can_enrol`, `one_row_per_entity`, `destination_table`, `provenance`. `columns` contains source keys plus `output_action` in `pseudonymise`, `retain`, `drop`; exactly the declared `id_column` must use `pseudonymise`, no other column may. `record_keys` is unchanged. `crosswalks` retains relation/namespace/column/provenance fields without `validation_status`. | Successful call sites can migrate mechanically. For one development cycle, exact current-master schemas map `bridge` to `pseudonymise`, `retain`/`retain_restricted` to `retain`, `drop` to `drop`, and ignore `privacy_class`/`validation_status` with one deprecation warning. `review` and `derive` have no executable meaning and remain errors. Output objects always use the new schema; saved old objects must be rebuilt. |
| `epi_sec_identity_registry_init()` | `con`, `registry_schema`, `token_prefix = "E"`, `n_bytes = 24`, `mode = c("audit", "apply")`; result contains `status`, `mode`, `writes`, `registry_schema`, `schema_restricted`, `next_action`, `metadata`, `objects`. | Retain all arguments, class, token settings, object names, physical registry schema, inspect/create behaviour and atomic transaction. Remove `PUBLIC` checks/revocations, ownership-as-policy gating and `schema_restricted`. A compatible registry is defined by relation kind, exact columns, constraints, version and immutable token settings; PostgreSQL permission failures are sanitised database errors. Statuses are `initialisation_required`, `incompatible`, `ready`. | Successful `ready` and `initialisation_required` consumers remain stable except for removal of `schema_restricted`. Current `blocked` for incompatible objects becomes `incompatible`; current `blocked` solely for `PUBLIC` access disappears. Physical registry version remains 1 because table structure and constraints do not change. |
| `epi_sec_pseudonymise_db()` | Existing arguments include audit/apply, duplicate and destination choices plus `sensitive_issues`; result uses `blocked`/`blocking`, governance conditions, marked diagnostics and a sensitivity-bearing manifest. | Retain existing arguments and defaults, add `include_issue_values = FALSE`, and retain `sensitive_issues = NULL` temporarily as a deprecated compatibility alias in its existing positional slot. Remove schema/crosswalk `PUBLIC` gates and privilege revocations. Consume neutral `output_action`. Audit always returns `audit_complete`; apply returns `not_written` when technical error-severity issues prevent a commit and `complete` after commit. Issue severity is `error` or `warning`. Requested values appear in an ordinary `issue_values` data frame; remove the sensitivity flag, redaction class/methods and inferred sensitivity manifest field. | Classes, successful statuses, aggregate audit tables, output dictionary/catalogues, token mapping, destination tables and write semantics remain stable. Failed-status values, issue columns, manifest columns and diagnostic component names change. For one cycle the deprecated argument maps to `include_issue_values`; when used, a deprecated `sensitive_issues` list alias may point to the same ordinary data frame so current extraction does not silently lose requested values. |
| `epi_sec_identity_universe_spec()` | `sources`, `normalization = "identity"`, `validity_regex = NULL`; six-column sources require `validation_status = "confirmed"`; returns a fingerprinted version-1 spec. | Retain arguments and class. Sources contain `source_schema`, `source_table`, `id_column`, `identity_namespace`, `provenance`; remove confirmation. Preserve two-or-more unique ordinary relation declarations, one namespace, exact identity normalisation, optional explicit regex, deterministic ordering, metadata-only contents and fingerprinting. Bump contract to `identity-universe-2`. | This export is absent from release `0.3.0`, so no released contract breaks. One-cycle construction accepts the exact six-column current-master input, ignores `validation_status` with a deprecation warning and returns the five-column version-2 object. Saved version-1 objects must be regenerated because their fingerprint covers the old schema. |
| `epi_sec_identity_universe_db()` | `con`, `spec`, audit/materialise, destination, `existing = "error"`, timeouts; result uses `blocked`/`blocking` and requires/revokes restricted-schema privileges. | Retain arguments, class, aggregate schemas, exact union/overlap calculations, output table columns, uniqueness, lock, snapshot and rollback behaviour. Remove schema `PUBLIC` refusal and table revocation. Audit always returns `audit_complete`; materialise returns `not_written` for technical error-severity issues and `complete` after commit. Source/namespace row statuses use `ready`, `warning`, `error`; issue severity uses `warning`, `error`. | Successful current consumers remain stable except for contract version/fingerprint. Current `blocked` result checks must migrate to `not_written` for materialisation or inspect `issues$severity` after audit. This interface is not in release `0.3.0`; repository callers and tests are the only visible consumers. |

## Exact Metadata Schemas

### Linkage tables

```text
source_schema, source_table, id_column, identity_namespace,
can_enrol, one_row_per_entity, destination_table, provenance
```

Exactly one `can_enrol = TRUE` row remains required because the registry algorithm needs one namespace from which unseen canonical entities may be created. This is a mapping rule, not authorisation. Unique source keys, unique destination names, non-empty identifiers/namespaces/provenance and non-missing Boolean declarations remain structural requirements.

### Linkage columns

```text
source_schema, source_table, source_column, output_action
```

`output_action` is exactly `pseudonymise`, `retain` or `drop`. Every selected current dictionary column must have one action. The declared `id_column` is the only `pseudonymise` row and is replaced by the generated token; `retain` rows are copied; `drop` rows are omitted. The package neither labels nor treats other retained columns as direct, quasi, sensitive or non-sensitive.

### Record keys

```text
source_schema, source_table, key_column, key_order
```

This schema and its ordered uniqueness rules remain unchanged. Every key column must use `output_action = "retain"`; a table declared `one_row_per_entity = TRUE` cannot also declare keys. These checks prevent ambiguous duplicate reconciliation.

### Crosswalks

```text
crosswalk_schema, crosswalk_table, alias_namespace, alias_id_column,
canonical_namespace, canonical_id_column, provenance
```

Crosswalk relation/type/collation, namespace, target, conflict, chain/cycle, blank-ID and immutable registry-assignment checks remain. `validation_status` and `PUBLIC` privilege tests disappear.

### Identity-universe sources

```text
source_schema, source_table, id_column, identity_namespace, provenance
```

The contract remains value-free. Extra columns remain errors because accepting arbitrary columns could accidentally embed identifiers in a portable specification; this is a structural and uncontrolled-disclosure safeguard, not a PII classifier.

## Restriction And Output Classification Inventory

| Current behaviour | Class | Decision and reason |
| --- | --- | --- |
| Pseudonym input must be a non-empty character/numeric/factor vector with unique, non-missing values. | 1 | Keep; one input-to-token row per identifier is otherwise undefined or ambiguous. |
| Tokens use `openssl::rand_bytes()`, at least 16 bytes, a caller prefix, bounded collision retries and a final uniqueness check. | 2 | Keep; this is the cryptographic and uniqueness contract. |
| Optional bridge CSV write. | 3 | Keep; writing and its path are caller-selected. |
| Existing bridge-file refusal unless `overwrite = TRUE`. | 2 | Keep; this prevents accidental replacement. |
| Dictionary/scaffold shape, active-table selection and exact metadata columns. | 1 | Keep; these establish deterministic portable configuration. |
| Rejection of unexpected value-bearing fields in portable linkage/universe metadata. | 2 | Keep; this avoids uncontrolled identifier inclusion without classifying a field as PII. |
| `unreviewed`/`pending`/`confirmed` statuses and specification refusal based on them. | 4 | Remove; a successfully validated structural specification is sufficient and carries no permission meaning. |
| `privacy_class` values and the rule that every additional direct identifier must be dropped. | 4 | Remove; the package cannot classify columns or override the caller's requested retention. |
| Caller selection of pseudonymise/drop/retain behaviour. | 3 | Retain as `pseudonymise`/`drop`/`retain`. |
| Exactly the declared identifier uses `pseudonymise`, and every selected column has one supported action. | 1 | Keep; identifier replacement and projection must be internally consistent. |
| Exactly one enrolment source, exact namespace membership, one-row-per-entity declarations, record-key ordering and retained keys. | 1 | Keep; removing these can split/merge identities incorrectly or make duplicate results ambiguous. |
| Crosswalks point to the enrolment namespace and declare at most one relation per alias namespace. | 1 | Keep; these define one exact mapping. |
| Crosswalk specifications contain relation metadata rather than identifier rows. | 2 | Keep; database-resident values are not required in portable configuration and their exclusion prevents uncontrolled disclosure. |
| Aggregate-only scaffold/spec print methods. | 3 | Keep concise summaries, but replace review/confirmation/audit-first wording. Not printing full components is ordinary S3 presentation, not sensitivity inference; users retain normal list access. |
| Registry schema must exist and apply must own its transaction. | 2 | Keep; the function does not create schemas and cannot safely join a caller-managed transaction. |
| Registry schema/table `PUBLIC` checks, `schema_restricted`, automatic revocations and access-based `blocked`. | 4 | Remove; PostgreSQL grants are caller/administrator configuration and server outcomes. |
| Registry relation names, ordinary-table kinds, exact columns/types/collations/defaults, primary/foreign/check constraints, version and immutable token settings. | 2 | Keep; these prevent incompatible registry reads and writes. |
| Registry object ownership as a compatibility requirement. | 4 | Remove for ordinary use; rely on the connected role's actual PostgreSQL privileges. Ownership checks remain only where the package is explicitly asked to replace an existing destination. |
| Registry creation in one repeatable-read transaction with an inside-transaction empty-state recheck. | 2 | Keep; this prevents partial or concurrent incompatible initialisation. |
| Source, registry and output schemas must exist and be mutually distinct. | 2 | Keep; separation prevents a requested replacement from targeting source or registry relations. This rule is about destructive target identity, not access approval. |
| Source/crosswalk relations must be ordinary tables; identifier types/families and text collations must support exact comparison. | 1 | Keep; unsupported or nondeterministic comparison can produce a wrong mapping. |
| Complete current dictionary/column-action coverage, token-column non-collision, referenced catalogue validity and equality-comparable retained columns. | 1 | Keep with neutral errors/issues; these are projection and duplicate-comparison prerequisites. |
| Schema/table `PUBLIC` checks for registry, output and crosswalks and automatic output-table revocation. | 4 | Remove; the package neither refuses nor changes grants. Missing read/create/drop privileges surface through the sanitised database boundary. |
| Null/blank/unmatched identifiers, crosswalk conflicts/missing targets/chains, missing record keys and conflicting payloads prevent writes. | 1 | Keep; proceeding would lose rows, map identities incorrectly or select an arbitrary observation. Report them as error-severity technical issues. |
| Exact duplicate `report`/`drop`, destination `error`/`replace`, audit/apply and caller timeout values. | 3 | Keep; these are explicit operation choices. Collision and concurrency enforcement are separately classified as data-integrity behaviour. |
| Replace only an owned ordinary non-partition table without dependencies and never use `CASCADE`. | 2 | Keep; it bounds the destructive target and prevents dependent-object damage. |
| Advisory session-lock acquisition, transfer to transaction locks, deterministic ordering, bounded wait, failed-unlock cleanup, rollback and post-write row reconciliation. | 2 | Keep unchanged; these are core concurrency and atomicity safeguards. |
| Registry aliases/entities/runs and outputs commit in one transaction; sources are unchanged. | 2 | Keep unchanged. |
| Native PostgreSQL/driver warnings and errors become fixed `epi_sec_database_condition` messages. | 2 | Keep; the boundary prevents credentials, SQL parameters or row values from escaping through uncontrolled server detail while still reporting a technical failure. |
| `epi_sec_governance`, policy `blocked` statuses, `blocking` severity and audit-first next actions. | 4 | Remove. Use ordinary validation errors before database work, `error`/`warning` issue severities during inspection, `audit_complete` for completed inspection and `not_written` when a requested write does not commit. |
| `sensitive_issues`, `epi_sec_sensitive_issues`, custom print/`str` redaction and issue/manifest sensitivity flags. | 4 | Replace with explicit `include_issue_values`, ordinary `issue_values`, and `output_type = "pseudonymised_table"`; do not infer or hide sensitivity. Default omission means the caller did not request values, not that episcout classified them. |
| Universe requires two unique sources, one namespace, exact identity normalisation and supported types/collations. | 1 | Keep; these define the set operation and exact comparison. |
| Optional universe validity regex. | 3 | Keep as the caller's explicit rule; episcout does not infer one from observed identifiers. |
| Universe null/blank/regex-invalid identifiers and normalisation collisions prevent materialisation; duplicates and empty sources warn. | 1 | Keep the substantive distinctions and aggregate counts, replacing `blocked`/`blocking` terms with neutral technical states. |
| Universe distinct-union and pairwise-overlap arithmetic. | 1 | Keep unchanged. |
| Universe read-only repeatable-read audit, unique destination, advisory lock, revalidation, unique constraint and rollback. | 2 | Keep unchanged. |
| Universe output schema must be restricted from `PUBLIC` and created table privileges are revoked. | 4 | Remove; the connected role's configured PostgreSQL permissions govern the operation. |
| Documentation says audit/approval/access review is permission to proceed or that output is safe/unsafe to disclose. | 4 | Remove. Documentation may state observable technical properties, including that pseudonymisation is reversible through the registry and is not anonymisation, while leaving permissions and use decisions to the caller. |

## Neutral Result, Issue And Condition Model

| Surface | Current | Replacement |
| --- | --- | --- |
| Registry top-level status | `blocked`, `initialisation_required`, `ready` | `incompatible`, `initialisation_required`, `ready`; `PUBLIC` access produces no package status. |
| Pseudonym audit status | `blocked` or `audit_complete` | Always `audit_complete` when inspection completes; consult error/warning issues. |
| Pseudonym apply status | `blocked` or `complete` | `not_written` when expected technical issues prevent commit; `complete` after commit. Infrastructure/authentication failures remain errors. |
| Universe audit status | `blocked` or `audit_complete` | Always `audit_complete` when inspection completes; consult error/warning issues. |
| Universe materialise status | `blocked` or `complete` | `not_written` or `complete`. |
| Issue severity | `blocking`, `warning` | `error`, `warning`. An error means the requested write cannot produce the defined technical result. |
| Source/namespace status | `blocked`, `warning`, `ready` | `error`, `warning`, `ready`. |
| Internal rollback conditions | `epi_sec_blocked`, `epi_sec_identity_universe_blocked` | `epi_sec_no_write`, `epi_sec_identity_universe_no_write`; these remain internal carriers caught before return. |
| Governance condition | `epi_sec_governance` | Remove. Portable malformed input uses an ordinary precise error; runtime coverage drift becomes a technical issue or validation error according to whether a complete audit result can still be formed. |
| Database condition | `epi_sec_database_condition` | Retain the class and sanitised fixed-message boundary; remove approval/restricted-policy wording from messages. |
| Diagnostic-value class | `epi_sec_sensitive_issues` with print/`str` redaction | Remove class and S3 registrations; an explicitly requested `issue_values` is an ordinary data frame. |

`audit_complete` asserts only that the read-only inspection ran against one snapshot. It is not permission and it does not imply that `issues` is empty. Direct apply/materialise remains supported without a prior call; the write path repeats the same inspection inside its owned protected transaction.

## Diagnostic Values

`include_issue_values = FALSE` returns aggregate/value-free issues only. `TRUE` requests the identifier values currently available for `invalid_identifier` and `unmatched_identifier`; the values are returned in ordinary component `issue_values` with `issue_code`, source relation/column metadata and `source_value`. The implementation must not begin collecting record-key payloads, arbitrary source values or crosswalk rows merely because diagnostics were requested. The result print method may remain a concise aggregate summary, but direct printing or `str()` of `issue_values` must behave like an ordinary data frame.

The deprecated `sensitive_issues` argument is a compatibility alias only. It makes no classification decision, emits one migration warning, and cannot conflict with an explicit `include_issue_values` choice. The `sensitive` issue column, manifest `sensitivity` column, `sensitive` attribute and redacting S3 methods disappear.

## Compatibility Assessment

Release `0.3.0` contains `epi_sec_pseudonym()`, `epi_sec_linkage_scaffold()`, `epi_sec_linkage_spec()`, `epi_sec_identity_registry_init()` and `epi_sec_pseudonymise_db()`. The identity-universe exports were added after that release. Current `master` also contains an unreleased four-component linkage schema introduced after `0.3.0`; it already rejects released three-component linkage objects with migration guidance.

The repository contains no non-test package caller beyond `epi_sec_pseudonymise_db()` calling registry initialisation. Visible user-style consumers are `vignettes/longitudinal-pseudonymisation.Rmd` and `inst/examples/db-to-report/walkthrough.R`; README, generated help, package help and EDA documentation cross-reference the functions. No external consumer implementation is visible in the repository.

Changes that do not break current technical consumers include retaining all seven export names, existing primary arguments/defaults, result S3 class names, successful `ready`/`initialisation_required`/`audit_complete`/`complete` statuses, registry physical schema/version, token formats, aggregate count tables, output table schemas, exact matching, duplicate choices and destination behaviour. Changes that require migration are the metadata component columns, error-path status/severity values, registry `schema_restricted`, issue/manifest sensitivity fields, requested diagnostic component/class, and universe version/fingerprint.

Compatibility adapters are bounded to one development cycle and translate only deterministic syntax. They must not honour a legacy confirmation or privacy value as permission, infer a replacement action, or preserve a deprecated result field whose value would be misleading. Serialized linkage and identity-universe specification objects must be regenerated through their constructors.

## Transitive Helper And S3 Inventory

| Group | Helpers/classes reached by the seven exports | Disposition |
| --- | --- | --- |
| Token generation and one-vector bridge | `sec_generate_tokens`; `epi_sec_pseudonym` result tibble | Keep unchanged except neutral documentation. |
| Linkage schemas, construction and parsing | `linkage_table_columns`, `linkage_column_policy_columns`, `linkage_record_key_columns`, `linkage_crosswalk_columns`, `read_linkage_csv_or_data`, `validate_linkage_columns`, `normalise_linkage_char_cols`, `parse_linkage_logical`, `parse_linkage_positive_integer`, `linkage_source_key`, `empty_linkage_tables`, `empty_linkage_columns`, `empty_linkage_record_keys`, `empty_linkage_crosswalks` | Rename the column-policy schema helper to an output-action helper; remove status/privacy fields; otherwise keep strict parsing, exact columns, key construction and empty schemas. Add bounded legacy normalisation. |
| Linkage validators | `validate_linkage_tables`, `validate_linkage_column_policy`, `validate_linkage_record_keys`, `validate_linkage_crosswalks` | Remove confirmation/privacy checks; retain structural mapping, unique destination, enrolment, key and crosswalk namespace checks. Rename column-policy validation to output-action validation. |
| Dictionary dependencies | `validate_dictionary_shape`, `validate_dictionary_values`, `dictionary_key_columns`, `dictionary_key`, `dictionary_source_columns`, `dictionary_curated_columns`, `dictionary_removed_fields`, `validate_dictionary_choice`, `validate_dictionary_geo`, `epi_eda_validate_spec`, `eda_removed_scaffold_fields`, `parse_eda_spec_logical`, `validate_eda_geo_spec`, `validate_eda_spec_ranges`, `eda_geo_resolve_crs`, `eda_geo_spec_fields`, `epi_geo_require`, `eda_geo_crs_value`, `epi_geo_crs`, `epi_geo_namespace_available`, `validate_catalogues` | Keep shared semantic/storage validation. No privacy inference is present in these current helpers. |
| Registry structure and creation | `sec_registry_tables`, `sec_registry_version`, `sec_registry_inspect`, `sec_registry_structure_ok`, `sec_registry_create`, `sec_registry_assert_settings`, `sec_registry_result`, `sec_empty_registry_metadata`, `sec_registry_object_frame` | Keep physical structure, version, constraints, immutable settings and atomic create. Remove `PUBLIC`/ownership policy from inspection and remove revocations/result restriction field. |
| Shared PostgreSQL boundary | `validate_postgres_connection`, `sec_connection_is_transacting`, `sec_database_boundary`, `sec_scalar_text`, `sec_whole_number`, `sec_require_schema`, `sec_quote_identifier`, `sec_quote_table`, `sec_quote_literal`, `sec_relation_state`, `sec_source_columns`, `sec_id_collation_deterministic`, `sec_identifier_family`, `sec_comparable_udt_names`, `sec_destination_state` | Keep. Neutralise messages. Ownership remains in `sec_destination_state` only for explicit replacement. |
| Privilege helpers | `sec_schema_is_public`, `sec_table_is_public` | Remove from all retained workflows and delete when no remaining caller exists. PostgreSQL permission failures flow through `sec_database_boundary`. |
| Pseudonym context and validation | `sec_pseudonym_context`, `sec_minimal_context`, `sec_validate_privacy_rows`, `sec_validate_catalogues`, `sec_validate_ns_families`, `sec_validate_crosswalks_db`, `sec_crosswalk_union`, `sec_configuration_hash` | Replace privacy validation with output-action validation; remove governance and `PUBLIC` checks; keep exact coverage, catalogue, type-family, relation/column/collation, crosswalk and deterministic hash behaviour. |
| Pseudonym audit and mapping | `sec_pseudonym_audit`, `sec_mapping_ctes`, `sec_unmatched_query`, `sec_duplicate_audit`, `sec_record_key_missing`, `sec_crosswalk_audit`, `sec_empty_issues`, `sec_issue` | Keep calculations and aggregate schemas. Rename issue severities/messages and remove `sensitive`. |
| Pseudonym diagnostics and rollback | `sec_empty_sensitive_issues`, `sec_sensitive_rows`, `sec_governance_stop`, `sec_governance_audit`, `sec_stop_blocked`, `sec_pseudonym_result`; conditions `epi_sec_governance`, `epi_sec_blocked` | Replace the first two with ordinary issue-value helpers; delete governance helpers/condition; replace rollback carrier and result status as specified. |
| Pseudonym writes and concurrency | `sec_apply_registry`, `sec_apply_outputs`, `sec_generate_registry_tokens`, `sec_lock_keys`, `sec_acquire_session_locks`, `sec_acquire_transaction_locks`, `sec_release_session_locks`, `sec_output_dictionary`, `sec_output_catalogues`, `sec_output_manifest`, `sec_registry_requested_setting` | Keep registry/output atomicity, collision handling, locks, reconciliation, dictionary/catalogue production and run metadata. Use `output_action`; remove privilege revocation and sensitivity manifest field. Remove `sec_registry_requested_setting` only if confirmed unused after implementation. |
| Identity-universe contract and SQL | `universe_source_columns`, `universe_validate_spec`, `identity_universe_timeout`, `identity_universe_transaction`, `universe_context`, `universe_source_predicates`, `universe_union_sql`, `universe_source_audit`, `universe_namespace_audit`, `universe_overlap_audit`, `universe_audit`, `eda_postgres_identifier`, `eda_postgres_fingerprint` | Keep exact set arithmetic, type/collation checks, timeouts, snapshot and fingerprint; remove confirmation field and bump contract version. |
| Identity-universe issues, destination and write | `identity_universe_empty_issues`, `identity_universe_issue`, `identity_universe_issues`, `identity_universe_is_blocked`, `universe_validate_destination`, `universe_add_destination_issue`, `identity_universe_create`, `identity_universe_stop_blocked`, `identity_universe_result`; condition `epi_sec_identity_universe_blocked` | Rename blocked predicates/condition/status/severity; keep null/blank/regex/collision, destination, lock, uniqueness and rollback logic; remove `PUBLIC` gate/revocation. |
| S3 presentation | `print.epi_sec_linkage_scaffold`, `print.epi_sec_linkage_spec`, `print.epi_sec_registry_result`, `print.epi_sec_pseudonymisation_result`, `print.epi_sec_identity_universe_spec`, `print.epi_sec_identity_universe_result`, `print.epi_sec_sensitive_issues`, `str.epi_sec_sensitive_issues` | Retain the six result/spec summary methods with neutral wording and updated counts. Delete the two diagnostic-redaction methods and their NAMESPACE registrations. |

## Known Callers And Documentation

| Consumer | Current coupling | Required implementation update |
| --- | --- | --- |
| `tests/testthat/test-epi_sec_pseudonym.R` | Cryptographic format, input validation and bridge overwrite only. | Remains green without behavioural edits. |
| `tests/testthat/test-sec-linkage.R` | Exact confirmation/privacy schemas and review/redaction print wording. | Replace fixtures with output actions, add legacy-adapter tests, retain every structural/key/crosswalk assertion. |
| `tests/testthat/test-sec-identity-universe.R` | Confirmed sources, blocked severity/status and aggregate printing. | First slice updates schemas/enums/version and proves no policy field controls execution. |
| `tests/testthat/test-sec-identity-universe-postgres.R` | Restricted destination setup, `PUBLIC` revocation assertion, blocked results and rollback forced at revoke. | First slice proves public grants are neither refused nor changed, preserves output/lock/rollback/reconciliation coverage, and forces rollback after a write step unrelated to permissions. |
| `tests/testthat/test-sec-pseudonymise-postgres.R` | Registry/output/crosswalk privilege policy, confirmation/privacy fixtures, blocked conditions, sensitive class redaction and all core technical invariants. | Slices 2/3 replace only governance assertions while preserving live coverage for registry shape, settings, exact mapping, families, crosswalks, duplicates, locks, replacement, rollback, row counts and sanitised database errors. |
| `vignettes/longitudinal-pseudonymisation.Rmd` | Uses all six PostgreSQL exports and assigns approval/access/disclosure meaning to audit, confirmation, restricted schemas and diagnostics. | Each behaviour slice makes only necessary local interface corrections; #269 later rewrites the complete operator journey after its issue body is reconciled. |
| `inst/examples/db-to-report/walkthrough.R` | Builds confirmation/privacy metadata and performs audit before apply. | Migrate to `output_action`; keep audit as an example, not a prerequisite. Do not add real data or credentials. |
| Generated help under `man/` | Mirrors current policy-heavy roxygen. | Regenerate from changed roxygen in every implementation slice; never edit Rd directly. |
| README, package help, EDA dictionary help and `PROJECT_MAP.md` | Cross-links and high-level capability statements. | Preserve links; update only factual interface/schema statements affected by an implementation slice. |

## Bounded Implementation Sequence

1. **Identity-universe neutral contract.** Implement the fully drafted first issue for `epi_sec_identity_universe_spec()` and `epi_sec_identity_universe_db()`: remove confirmation and restricted-schema policy, introduce version 2 and neutral result enums, preserve all set, type, lock, transaction, uniqueness and rollback behaviour, and update focused tests/roxygen/generated help plus only directly affected guide lines.
2. **PostgreSQL privilege neutrality for the stable registry path.** In one end-to-end slice, remove `PUBLIC`/ownership policy and automatic revocations from registry inspection/creation and pseudonymisation output/crosswalk handling; replace registry `blocked`/`schema_restricted` with structural `incompatible`; prove configured grants are unchanged; preserve physical registry version, source/registry/output separation, destination ownership for replacement, database-error sanitisation and all transaction/lock/reconciliation safeguards. Update tests and generated help together.
3. **Neutral linkage, projection, issues and diagnostics.** Change scaffold/spec metadata to `output_action`, remove validation/privacy fields and governance conditions, introduce neutral audit/apply issue semantics, add ordinary caller-selected issue values and migrate the specialised manifest. Include the bounded legacy adapters, update `epi_sec_pseudonym()` cross-reference prose, the walkthrough, focused/live tests, roxygen/Rd and necessary local vignette snippets together.
4. **Longitudinal guide reconciliation.** After all three behaviour slices are canonical and closed out, rewrite/re-authorise #269 against this contract. That documentation-only issue must remove its current requirement to preserve approval and blocker language while retaining accurate technical recovery, exact mapping, transaction and credential-disclosure limits.

Each behavioural slice requires a separate tracking issue and numbered implementation specification. Do not activate slice 2 until slice 1 is merged and closed out, or slice 3 until slice 2 is merged and closed out. A failed implementation can be reverted at its scoped commit/PR without changing registry table structure or deleting data; no migration mutates existing registry rows.

## Reconciliation Notes

- **#274:** Preserve its owner boundary: episcout performs requested technical calculations and writes but has no approval, privacy, disclosure, sharing or publication authority. Its comment superseding removal/relocation is honoured; all seven exports remain.
- **#275:** This design supplies the requested inventory and three-slice roadmap. Technical pseudonymisation, registry, linkage and identity-universe mechanics remain operational; `PUBLIC` grants become server outcomes; requested diagnostics are not redacted by a package sensitivity inference.
- **#269:** Keep the issue in `question` state. Its current body conflicts with #274/#275 by requiring approval, access-review, mandatory audit-first and blocker language. After implementation, rewrite its acceptance criteria around observable prerequisites, optional inspection, technical side effects, exact statuses/schemas, transaction recovery and the factual limit that pseudonymisation is not anonymisation. Do not restore removed gates in prose.
- **#249:** No package implementation is active during issue #276. Record the created first successor under #274/#275 before this design closes; reconcile #249 and repository queue records only through the normal lifecycle, without claiming this future contract is current package behaviour.
