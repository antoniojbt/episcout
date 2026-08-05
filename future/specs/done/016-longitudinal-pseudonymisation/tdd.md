# Test Design

Spec ID: `016-longitudinal-pseudonymisation`
Status: Implemented

Use small, hand-authored, unmistakably synthetic fixtures. Generate unique disposable schema suffixes and opaque identifier values at runtime. Independently derive expected counts, resolution and duplicate outcomes; do not generate expected fixtures through the implementation under test. Never use existing package datasets, realistic names, institutions, places, diagnoses, facilities, credentials, connection strings, local paths or source-system labels.

## Test Files

- `tests/testthat/test-security.R`
- `tests/testthat/test-security-linkage.R`
- `tests/testthat/test-security-postgres.R`

Exact filenames may be narrowed after inspecting current test organization. PostgreSQL integration tests require `EPISCOUT_TEST_POSTGRES=1`, a disposable database and explicit connection environment documented for maintainers. Ordinary local and CRAN checks skip live integration when the gate is absent. CI supplies a pinned PostgreSQL major version and turns the gate on in its mandatory integration job.

## Recorded Baseline

Before package-code changes on `feature/longitudinal-pseudonymisation`:

- Package-loaded lint: clean.
- Full tests: 1,573 pass; one existing macOS daylight-saving-time ambiguity failure at `tests/testthat/test-eda-prepare.R:223`.
- `scripts/check-local.sh`: fails from the same existing test.
- `scripts/check-cran.sh`: same failure plus existing notes for new-submission status, the vignette index, HTTP 403 responses from documented URLs and HTML tidy output.

Do not weaken the existing failing test or claim these inherited findings were caused or fixed by spec 016 without independent evidence.

## Baseline Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/check-local.sh
scripts/check-cran.sh
git status --short
```

## Linkage Scaffold And Specification

- [ ] `epi_sec_linkage_scaffold()` reads dictionary metadata only and never calls a row-reading database path.
- [ ] Scaffold table, record-key and crosswalk components have exact stable columns, types and deterministic order.
- [ ] Candidate source/ID metadata may be copied from the dictionary, while namespace, enrolment, row grain, keys, provenance and validation remain explicitly unreviewed.
- [ ] Empty/zero-table dictionaries preserve usable zero-row schemas and review instructions.
- [ ] Table selection/override validation rejects missing, duplicate, extra and malformed metadata without exposing values.
- [ ] `epi_sec_linkage_spec()` has exact formals, class and component order.
- [ ] Exactly one table must have `can_enrol = TRUE`; repeated enrolment rows remain allowed.
- [ ] Every table and crosswalk review status must be exactly confirmed and provenance non-empty.
- [ ] Source selections and destination names are unique and identifiers validate conservatively.
- [ ] Ordered record-key columns refer to selected tables, are unique/consecutive and cannot coexist with `one_row_per_entity = TRUE`.
- [ ] Crosswalk metadata contain no value columns, use declared namespaces, support reviewed same-namespace aliases and reject duplicates and static cycles.
- [ ] Printing is friendly, concise and contains no identifier or key values.

## Registry Unit And Live Tests

- [ ] Validate RPostgres connection, schema, token prefix, byte count, mode and PostgreSQL-only backend errors.
- [ ] Audit mode creates no schema object, table, row or privilege change.
- [ ] Audit distinguishes absent, complete-compatible, partial, incompatible-version, wrong-kind, wrong-owner and publicly creatable registry state.
- [ ] Apply creates/validates versioned `registry_metadata`, `namespaces`, `entities`, `aliases`, `runs`, `run_tables` with expected primary, unique and foreign-key constraints.
- [ ] Registry ID is random, immutable and contains no accepted project/study/institution field.
- [ ] Namespace/type-family and alias uniqueness constraints reject incompatible or conflicting state independently of R validation.
- [ ] Tokens are cryptographically random, prefix-conforming, globally unique and independent of identifiers.
- [ ] Bounded token-collision injection retries and then fails/rolls back safely when the bound is exhausted.
- [ ] Apply revokes `PUBLIC` table privileges and refuses a registry schema granting `CREATE` to `PUBLIC`.
- [ ] Existing compatible registry validation changes no immutable metadata.
- [ ] Native database errors that could contain values are sanitized in public results/conditions.

## Dictionary And Projection Gates

- [ ] Require exact current confirmed dictionary coverage for every selected source column.
- [ ] Block missing/extra/modified/unclassified/pending dictionary rows and source drift by aggregate, value-free issues.
- [ ] Require every selected ID to be a direct identifier with action `bridge`.
- [ ] Remove `bridge` and `drop`; preserve confirmed `retain` and `retain_restricted` types and order.
- [ ] Block unsupported `derive`, incompatible privacy/action pairs and token-column collisions.
- [ ] Validate referenced catalogues and return only definitions still referenced by retained outputs.
- [ ] Output dictionary replaces the ID position with the actual token metadata and confirmed generated provenance.
- [ ] Output dictionary/catalogues validate through current contracts and pass directly into `epi_eda_dictionary_spec()`.

## Exact Identity Semantics

- [ ] Text identifiers preserve case, leading zeros and non-empty whitespace exactly; null, empty and whitespace-only identifiers block.
- [ ] Integral identifiers match exactly without floating-point/R materialization.
- [ ] UUID identifiers use PostgreSQL UUID identity, including accepted lexical variants.
- [ ] Namespace type family is stored on first use and incompatible reuse is refused.
- [ ] The enrolment table creates entities only for exact new identifiers and repeated identifiers reuse one entity.
- [ ] Existing aliases and same-namespace matches reuse stable tokens across runs and selected tables.
- [ ] Dependent unmatched identifiers block without their values entering the result.
- [ ] Confirmed crosswalk aliases resolve to current-run enrolment identifiers or existing canonical registry aliases.
- [ ] Many reviewed aliases may resolve to one entity.
- [ ] Missing canonical targets, cycles and conflicting alias assignments block atomically.
- [ ] Unused crosswalk rows are reported by count only.
- [ ] No trimming, case folding, hashing for matching, similarity or probabilistic inference path exists.

## Longitudinal And Duplicate Semantics

- [ ] Repeated entity identifiers in all tables are valid and do not by themselves remove rows.
- [ ] `one_row_per_entity = TRUE` uses token alone and blocks different retained payloads for one token.
- [ ] Composite record keys use token plus declared key columns in reviewed order.
- [ ] Missing key components and unsupported equality types block before writes.
- [ ] Equal keys with `IS NOT DISTINCT FROM`-different payloads block without selecting or aggregating a row.
- [ ] Null payloads compare with PostgreSQL not-distinct semantics.
- [ ] Exact final-projection duplicates are counted and preserved under `report`.
- [ ] Exact final-projection duplicates are deliberately removed and reconciled under `drop`.
- [ ] Tables without a declared key inspect exact projected duplicates and warn that conflicts cannot be identified.
- [ ] Legitimate repeated events remain distinct.
- [ ] Per-table source, linked, projected, output and explicitly removed counts reconcile exactly.

## Result, Issues And Redaction

- [ ] Exact workflow formals/defaults, result class and component order match the SDD.
- [ ] Status is limited to `audit_complete`, `blocked`, `complete`; audit never reports complete.
- [ ] Fixed issue columns/types/order and severity/stage vocabularies remain stable on zero-row and populated paths.
- [ ] Expected governance/data findings return blocked results with exact next actions; malformed/unsupported/unsafe infrastructure states error actionably.
- [ ] Default `sensitive_issues = FALSE` never materializes source identifiers or key values in R.
- [ ] Explicit sensitive mode appends only the documented marked memory-only component and excludes it from print, manifest, hashes and persisted audits.
- [ ] Print methods show status, counts, write state, output location and next action without identifiers, keys, tokens or native database details.
- [ ] Scan returned character fields, printed output, captured conditions, manifest/audit tables, generated SQL and output relations for seeded secret/value markers.
- [ ] Metadata and manifests contain no credentials, connection strings, native SQL, local paths or project-specific labels.

## Audit, Transaction And Destination Safety

- [ ] Audit mode performs full read-only preflight and writes no registry/output/temp-persistent state.
- [ ] Source, registry and output schemas must be pre-existing, distinct and appropriately non-public.
- [ ] Apply rejects invocation inside a caller-managed transaction.
- [ ] Apply owns one `REPEATABLE READ` transaction and reruns every preflight within its snapshot.
- [ ] Concurrent source changes do not produce a mixed snapshot.
- [ ] Advisory locks are acquired in deterministic registry/destination order.
- [ ] Overlapping registry and destination runs serialize without deadlock.
- [ ] Bounded lock timeout returns a value-free blocked result and persists nothing.
- [ ] Every temporary staging relation is in `pg_temp` with `ON COMMIT DROP`.
- [ ] Injected failure after each registry/output write phase rolls back all changes.
- [ ] `existing = "error"` blocks any occupied destination without mutation.
- [ ] `existing = "replace"` replaces only caller-owned declared ordinary tables.
- [ ] View, foreign-owned object, dependency and unsupported relation kind block replacement.
- [ ] Generated SQL never uses `CASCADE`.
- [ ] Output retains source types/order for eligible columns and places the token at the ID position.
- [ ] Source row multiset, columns, constraints, comments, ownership and grants are identical before/after audit, successful apply and failed apply.
- [ ] Persisted successful run/audit metadata contain only approved value-free hashes, provenance and aggregate counts.

## Documentation And Usability

- [ ] Canonical vignette contains all 15 SDD sections in first-time-user order and ordinary-build chunks are unevaluated.
- [ ] README reaches the guide in one step from Features and Getting Started.
- [ ] Package, introductory, specification-first, inventory, dictionary and pseudonymisation help have reciprocal working links.
- [ ] File-scaffold README warns against exporting identifiable database rows merely to use EDA scaffolding.
- [ ] Every help page documents columns/types, defaults, statuses, returned schemas, no-write guarantees, blocker/error split, privacy limitations and recovery.
- [ ] A new user can run the complete disposable PostgreSQL walkthrough, audit before writing, interpret unmatched/duplicate blockers, locate output and verify token stability.
- [ ] The guide's duplicate decision table agrees with executable behavior.
- [ ] Documentation repeatedly and prominently says pseudonymised data remain restricted personal data and are not anonymous or automatically disclosure-controlled.
- [ ] No documentation claims control over database/driver/backup/administrator logs.
- [ ] Render the vignette, inspect it visually and verify `vignette(package = "episcout")` lists it.
- [ ] Audit all touched examples, fixtures, tests, messages and prose for real/project-specific names, paths, identifiers, credentials and unsupported privacy claims.

## CI And Integration

- [ ] Add one mandatory Ubuntu PostgreSQL integration job with a pinned PostgreSQL major version and `EPISCOUT_TEST_POSTGRES=1`.
- [ ] Inspect actual workflow/job names and reuse current R setup/cache/check conventions.
- [ ] Local and CRAN checks skip live integration unless explicitly enabled and state the reason.
- [ ] Disposable integration setup uses runtime schema suffixes and validates exact cleanup targets before dropping any test schema.
- [ ] The complete live matrix covers registry, identity families, crosswalks, duplicates, transactions, locks, replacement, non-mutation, redaction and EDA handoff.

## Independent Reviews

- [ ] A bounded independent review covers PostgreSQL constraints, privilege checks, transactions, snapshot semantics, advisory locking and replacement safety.
- [ ] A bounded independent review covers identifier/key/token leakage through ordinary R/database outputs, conditions, SQL and documentation.
- [ ] A bounded independent documentation usability review confirms discoverability, first-run clarity, blocker recovery and truthful privacy language.
- [ ] The primary implementer reconciles every finding and performs final integration/acceptance review.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'security|linkage|postgres|dictionary|eda-schema', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
EPISCOUT_TEST_POSTGRES=1 scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'security-postgres', reporter = 'summary')"
scripts/check-local.sh
scripts/check-cran.sh
git diff --check
```

Record exact commands, PostgreSQL version, pass/skip/failure counts, inherited versus new findings, documentation render evidence and genericity search evidence in `review.md`. Restore check-generated unrelated changes before handoff.
