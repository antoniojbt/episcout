# First Implementation Issue Draft

## Title

`[Refactor] Make the epi_sec identity-universe contract neutral and technical`

## Parent

- #274 — package authority boundary.
- #275 — retained `epi_sec_*` refactor roadmap.
- #276 — accepted interface inventory and implementation sequence.

## Outcome

Refactor `epi_sec_identity_universe_spec()` and `epi_sec_identity_universe_db()` so they define and execute an exact caller-requested PostgreSQL identifier-universe operation without confirmation, restricted-schema or package approval semantics. Preserve aggregate set calculations, exact typed identity, collision detection, uniqueness, transaction ownership, advisory locking, rollback and value-free ordinary results.

This is the first behaviour slice from spec `034-retained-epi-sec-technical-contract`. Create and activate implementation spec `035-identity-universe-technical-contract` before editing package code.

## Required Contract

### `epi_sec_identity_universe_spec()`

- Retain `sources`, `normalization = "identity"` and `validity_regex = NULL` arguments and the `epi_sec_identity_universe_spec` class.
- The current source schema becomes exactly `source_schema`, `source_table`, `id_column`, `identity_namespace`, `provenance`; remove `validation_status` and every `confirmed` requirement/print/doc claim.
- Preserve at least two unique relation declarations, one shared namespace, valid PostgreSQL identifier names, non-empty provenance, metadata-only contents, exact identity normalisation, optional explicit PostgreSQL regex, deterministic source order and SHA-256 fingerprint.
- Set `contract_version = "identity-universe-2"` so saved version-1 objects cannot be mistaken for the new schema.
- For one development cycle, accept only the exact current six-column source input as a deprecated construction form, ignore `validation_status` regardless of value, emit one migration warning and return the five-column version-2 object. Extra arbitrary columns and value-bearing fields remain errors.
- Reject saved version-1 or modified objects at the database boundary with concise regeneration guidance; do not infer or repair a fingerprint.

### `epi_sec_identity_universe_db()`

- Retain all arguments/defaults, the result class, aggregate table schemas and the materialised columns `identity_namespace`, `canonical_identifier`, `source_membership_count` with their uniqueness/check constraints.
- Audit remains the default optional technical inspection. A completed audit always returns `status = "audit_complete"`, `writes = FALSE`, and `issues` with severity `error`/`warning`; it is not permission and direct materialisation remains supported.
- Materialisation returns `status = "not_written"` when error-severity findings, an existing destination or lock timeout prevent commit, and `status = "complete"` only after commit. Infrastructure, authentication, unsupported type/collation, malformed argument and caller-transaction failures remain errors.
- Source and namespace status values become `ready`, `warning`, `error`. Replace `blocking` with `error`. Rename internal blocked predicates/conditions to neutral no-write terms without exposing transaction-control conditions to ordinary successful callers.
- Remove the output-schema `PUBLIC` check and the created-table `REVOKE`. Do not query, grant, revoke or otherwise change schema/table privileges. PostgreSQL permission denial is a sanitised server/technical error through the existing database boundary.
- Preserve schema existence, source/destination distinction, ordinary source relation checks, supported exact identifier families, deterministic text collation, regex validation, null/blank/invalid/collision handling, duplicate and empty-source warnings, pairwise overlap calculations, repeatable-read/read-only audit, statement timeout, session-to-transaction advisory-lock transfer and cleanup, inside-transaction revalidation, existing-destination refusal, source non-mutation, rollback and fixed value-free database errors.
- Keep ordinary results aggregate and value-free. Do not add identifier-value diagnostics in this slice.

## Compatibility

These exports are absent from release `0.3.0`; repository tests, generated help and `vignettes/longitudinal-pseudonymisation.Rmd` are the only visible consumers. Successful `audit_complete` and `complete` consumers remain stable. Current blocked-audit consumers must inspect `issues$severity == "error"`; materialisation no-write consumers use `not_written`. Version-1 specs must be regenerated. The temporary six-column constructor adapter is syntax-only and must not treat a confirmation value as authority.

## Files In Scope

- `R/epi_sec_identity_universe.R`
- Shared `sec_schema_is_public()`/database helpers only where required by this pair and without changing the later registry/pseudonymisation behaviour before its own slice.
- `tests/testthat/test-sec-identity-universe.R`
- `tests/testthat/test-sec-identity-universe-postgres.R`
- Roxygen-generated `man/epi_sec_identity_universe_spec.Rd` and `man/epi_sec_identity_universe_db.Rd`
- Only directly affected identity-universe fields/status examples and prose in `vignettes/longitudinal-pseudonymisation.Rmd`
- The new numbered implementation specification and normal lifecycle records.

## Acceptance Criteria

- [ ] Both retained exports and all current primary arguments remain.
- [ ] Version-2 specifications have the exact five-column metadata-only source schema and no confirmation/approval field or wording.
- [ ] Legacy `validation_status` is ignored with one deprecation warning and cannot change success/failure; arbitrary extra columns remain rejected.
- [ ] Audit completion is independent of issue presence; technical error/warning rows retain every current substantive code/count.
- [ ] Materialisation uses `not_written` for expected technical no-commit outcomes and `complete` only after commit.
- [ ] No identity-universe path inspects, refuses, or changes `PUBLIC` or role grants; configured PostgreSQL permissions determine whether SQL succeeds.
- [ ] Source relation/type/collation, exact union/overlap, null/blank/regex/collision, uniqueness, destination, timeout, lock, transaction, rollback and value-free error protections remain covered.
- [ ] The independent fixture `{A, B, B, C}` plus `{B, C, D}` still yields source distinct counts `3/3`, duplicate excess `1/0`, union `4`, single-source `2`, multi-source `2`, intersection `2` and directional coverage `2/3`.
- [ ] A disposable schema with explicit `PUBLIC` privileges materialises successfully when the connected role has permission, and before/after grant state is identical.
- [ ] A forced post-create non-permission failure rolls back the destination completely.
- [ ] Roxygen is regenerated from source, changed Rd matches formals/result schemas, and only the bounded guide section changes.
- [ ] Focused offline/live PostgreSQL tests, package lint, `scripts/check-local.sh`, `scripts/check-workflow-state.sh` and `git diff --check` pass; exact environment-dependent skips are reported.
- [ ] No real identifiers, production database, credential, package approval, privacy classification, disclosure decision or unrelated `epi_sec_*` refactor enters the diff.

## Verification

Run focused unit tests first, then the opt-in test against an approved disposable PostgreSQL environment:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'sec-identity-universe', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 PGHOST=127.0.0.1 PGPORT=55432 PGDATABASE=synthetic_records PGUSER=postgres scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'sec-identity-universe-postgres', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/check-local.sh
scripts/check-workflow-state.sh
git diff --check
```

## Exclusions

- No changes to `epi_sec_pseudonym()`, linkage scaffold/spec, registry initialisation, main pseudonymisation, Epidepot, core EDA or PostgreSQL role/grant administration.
- No new normalisation rule, source-value return, replacement mode, registry enrolment, pseudonym generation, backend or dependency.
- No broad longitudinal-guide rewrite; #269 remains paused until all behaviour slices are stable.

## Successor Disposition

After this slice merges and closes out, create the second tracker from spec 034's **PostgreSQL privilege neutrality for the stable registry path** boundary. Do not activate it before version-2 identity-universe closeout passes `scripts/check-workflow-state.sh`.
