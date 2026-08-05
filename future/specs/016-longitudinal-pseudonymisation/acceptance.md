# Acceptance

Spec ID: `016-longitudinal-pseudonymisation`
Status: Active

- [x] The owner-approved scope, exclusions, public interfaces, baseline findings and branch are captured before package-code changes.
- [x] SDD and TDD define exact review, privacy, identity, row-duplicate, transaction, destination, result, documentation and verification boundaries.
- [x] `epi_sec_pseudonym()` preserves its released behaviour and uses neutral synthetic documentation examples with reciprocal multi-table links.
- [x] `epi_sec_linkage_scaffold()` returns value-free draft table/key/crosswalk metadata and leaves every consequential semantic field explicitly unreviewed.
- [x] `epi_sec_linkage_spec()` requires confirmed provenance, exactly one enrolment table, valid record keys and metadata-only confirmed crosswalks.
- [x] Registry audit writes nothing and registry apply atomically creates/validates the versioned restricted registry with immutable random domain ID, namespace families, stable random tokens, constraints and public privilege removal.
- [x] No API accepts a project/study/institution identifier or embeds project-specific defaults, fixtures, messages or examples.
- [x] The reviewed current dictionary completely governs source coverage, ID bridging, dropped/retained columns, catalogue preservation and actual output dictionary metadata.
- [x] Text, integral and UUID identifiers retain their exact documented PostgreSQL semantics, reject missing/blank identities and refuse namespace-family changes.
- [x] Only exact same-namespace or confirmed database-resident crosswalk matches occur; many reviewed aliases may share an entity, while conflicts, cycles, missing targets and unmatched dependents block.
- [x] Source identifiers remain in PostgreSQL by default; token generation uses unpaired R randomness and database-local association, and ordinary results/conditions/audits/manifests/SQL/output contain no source identifiers or key values.
- [x] The optional sensitive issue path is explicit, memory-only, visibly marked and excluded from printing, persistence, manifests and hashes.
- [x] Repeated entity identifiers remain valid longitudinal records; token/composite keys and exact projected duplicates follow the documented conflict/report/drop contract without inferred winners or aggregation.
- [x] Every table reconciles source, linked, projected, output and deliberately removed duplicate counts with no unexplained row change.
- [x] Audit mode writes nothing and returns actionable `audit_complete` or `blocked` results through the fixed redacted issue schema.
- [x] Apply rejects caller-owned transactions, owns one repeatable-read transaction, rechecks preflight, acquires deterministic bounded advisory locks and rolls back every registry/output write on failure.
- [x] Destination replacement is limited to exact caller-owned ordinary tables without `CASCADE`; ownership, kind or dependencies block safely.
- [x] Source content and database metadata remain unchanged after audit, successful apply and failed apply.
- [x] Successful apply persists only value-free run/configuration/provenance/count metadata and produces stable pseudonyms across runs and tables.
- [x] Output dictionaries/catalogues validate and hand directly into `epi_eda_dictionary_spec()`.
- [x] Stable print methods and help pages give first-time users statuses, counts, write state, output location, exact next actions, failure/blocker distinction and recovery without sensitive values.
- [x] The dedicated 15-section vignette is discoverable from README in one step and from all required package/database/dictionary/pseudonymisation documentation.
- [x] Documentation states prominently that pseudonymised data remain restricted personal data and are not anonymous or automatically disclosure-controlled, and makes no unsupported infrastructure-logging claim.
- [x] Unit and mandatory pinned-PostgreSQL integration tests cover registry integrity, exact identity, aliases, duplicates, concurrency, rollback, replacement, source invariants, redaction and EDA handoff.
- [x] All test/example fixtures are hand-authored neutral structures with runtime-generated opaque identifiers/schema suffixes and validated cleanup targets.
- [x] Independent PostgreSQL integrity/concurrency, leakage and documentation-usability reviews find no unresolved blocker.
- [x] The disposable PostgreSQL walkthrough is represented by the live integration suite; the rendered 15-section vignette was inspected structurally, independently reviewed and listed by the installed package.
- [x] Software verification, truth and semantics, copy-edit and render-and-release checklists are completed; analysis/statistics is applied only to reconciliation and handoff claims.
- [x] Focused tests, live PostgreSQL integration, package-loaded lint, local/CRAN checks and `git diff --check` pass or every inherited/external limitation is recorded without weakening requirements.
- [x] A repository-content audit finds no real/project-specific material, credential, developer path, observed identifier or unsupported privacy claim in touched material.
- [x] Review notes contain exact verification and independent-review evidence; TODO/changelog/spec status are reconciled at checkpoints and closeout.
- [ ] The focused change is committed and pushed on `feature/longitudinal-pseudonymisation`, and a draft PR titled `[Feature] Add longitudinal pseudonymisation workflow` records behaviour, checks, compatibility, coverage if measured, genericity audit, privacy limitations and unresolved issues.
- [x] No release, tag, schema creation, role grant, backup management or server-log configuration is performed.
