# Test Design

Spec ID: `038-longitudinal-pseudonymisation-documentation`
Status: Completed

## Independent Basis

The accepted documentation contract comes from owner-authorised `issue-269`, completed `spec-034` and final implemented source at exact `commit-d79dd3c`. Focused unit tests independently assert exact public component schemas and neutral statuses; disposable PostgreSQL tests assert aggregate counts, stable mappings, duplicate outcomes, privilege neutrality, lock cleanup, rollback and source/output preservation. The rendered guide must agree with those sources without deriving a permission or disclosure conclusion from successful execution.

## Baseline

- [x] The clean task branch and fetched `origin/master` equal exact `commit-d79dd3c05ea1c64db2acd27b5b0af6b8193646b6`.
- [x] GitHub confirms `issue-269` is open, `PR-295` merged at the exact base, `PR-291` merged as `commit-0d9b302` and closeout `PR-292` merged as `commit-b80abc5`.
- [x] The initial workflow-state check found structurally consistent manifests and no competing active/review implementation; live mode was limited only by the disposable clone lacking a remote named `upstream`.
- [x] Runtime introspection confirms exactly seven exported `epi_sec_*` functions and their current formals.
- [x] Focused offline tests pass with only the four explicit live PostgreSQL gates skipped.

## Documentation Reconciliation

- [x] Every public formal, class, component/status schema and error statement in the guide matches source and focused tests.
- [x] Audit is described as optional inspection, not a mandatory permission gate; apply and materialisation are caller-selected requests that repeat protected checks.
- [x] Registry and output schema examples are neutral caller-selected/disposable names, not package-required restricted classifications.
- [x] Registry reversibility, pseudonymisation-not-anonymisation and no automatic disclosure-control warnings remain explicit.
- [x] PostgreSQL credentials, permissions, storage, backups, retention and logging remain external responsibilities without package approval language.
- [x] Duplicate report/drop, conflicting-key, exact identity, stable-token-domain, destination replacement and EDA handoff descriptions match observable behaviour.
- [x] Transaction ownership, advisory-lock acquisition/transfer/cleanup, rollback, `not_written` recovery and sanitised error recovery match source and live tests.
- [x] Examples contain only neutral runtime-generated synthetic identifiers and no credentials, keys, private paths or record-level outputs.
- [x] Directly related roxygen/Rd changed only for the verified registry-help classification mismatch.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'epi_sec_pseudonym|sec-identity-universe|sec-linkage|sec-registry|sec-pseudonymise-postgres', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 PGHOST=127.0.0.1 PGPORT=55432 PGDATABASE=synthetic_records PGUSER=$USER scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'sec-identity-universe-postgres|sec-pseudonymise-postgres', reporter = 'summary')"
scripts/rscript_env_caller.R -e "output <- rmarkdown::render('vignettes/longitudinal-pseudonymisation.Rmd', output_dir = tempdir(), quiet = TRUE); cat(output, '\n')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::document()"
scripts/check-local.sh
scripts/check-workflow-state.sh
git diff --check
```

## Render Inspection

The temporary HTML from the current source contains 18 headings, four tables and 18 code blocks. Structural and full-text inspection found the complete table of contents and required technical/confidentiality boundaries, and found none of the targeted stale governance, approval, restricted-schema or replacement-authority phrases. No graphical browser is installed in the execution environment, so layout inspection is limited to the rendered HTML structure and text. The render is a self-check and is not publication approval.

## Recovery Verification

Rendering created no tracked artefact. The disposable PostgreSQL run left zero runtime schemas, roles and advisory locks. `devtools::document()` generated the intended registry Rd change and also exposed unrelated author-markup drift in non-`epi_sec_*` Rd files; that pre-existing drift was removed from the task diff. Final clean-worktree verification remains pending until commit.
