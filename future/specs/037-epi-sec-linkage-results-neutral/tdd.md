# Test Design

Spec ID: `037-epi-sec-linkage-results-neutral`
Status: Active

## Independent Basis

The accepted contract is defined by owner-authorised `issue-285` and completed `spec-034`; predecessor `PR-289`/`PR-290` establishes that privilege neutrality is canonical and that linkage/result/diagnostic behaviour remains untouched. Independently authored small metadata fixtures define exact component schemas and projection actions. Existing live synthetic fixtures independently define the expected mappings, counts, duplicate outcomes, source preservation, lock cleanup, rollback and output reconciliation.

## Baseline

- [x] Exact task branch, `origin/master` and `upstream/master` equal canonical `commit-6ba8f25213569aaffb55f8c10ef544a4800f6de6`.
- [x] Online `scripts/check-workflow-state.sh` passes before activation.
- [x] Focused offline linkage/pseudonymisation tests pass; exactly three explicitly gated PostgreSQL tests skip.
- [x] `issue-284`, `PR-289`, `PR-290`, their merged diffs/checks and the predecessor specification are reconciled before package edits.

## Linkage Tests

- [x] Scaffold returns the exact four neutral schemas, blank `output_action` values and no confirmation/privacy fields.
- [x] New-schema construction preserves all table, key, crosswalk, ordering, CSV and metadata-only validation while enforcing exactly one identifier `pseudonymise` action and retained record keys.
- [x] Exact legacy tables/columns/crosswalk schemas translate with one warning, ignore status/privacy values, map only executable actions and return no legacy fields.
- [x] `review`, `derive`, arbitrary extra/value-bearing fields and saved legacy database-bound objects fail with concise migration guidance.
- [x] Printers use neutral technical summaries, expose no component values automatically and do not imply confirmation, review or audit permission.

## Result And Diagnostic Tests

- [x] Completed audit returns `audit_complete` with error/warning issues and writes nothing; apply returns `not_written` for preflight errors, lock timeout and protected-transaction error findings.
- [x] Issue severities are only `error`/`warning`, the issue schema has no sensitivity flag and the printer reports error counts without blocker language.
- [x] `include_issue_values = FALSE` omits values; `TRUE` returns an ordinary `issue_values` data frame whose direct `print()`/`str()` displays ordinary contents.
- [x] The deprecated alias warns once, maps true/false compatibly, may return the ordinary legacy-named component only when requested, and errors on an explicit conflicting new argument.
- [x] No governance or redacting S3 condition/class remains registered, and malformed runtime coverage/catalogue state produces a precise ordinary error.
- [x] Manifests use exact `output_type = "pseudonymised_table"` and no inferred sensitivity field; output dictionary/catalogue validation remains accepted.

## Preserved PostgreSQL Tests

- [x] Exact linkage, crosswalk, registry mapping and record-key outcomes remain unchanged for the existing synthetic fixtures.
- [x] Exact duplicate report/drop, destination error/replace, token stability/collision, source non-mutation and row reconciliation remain unchanged.
- [x] Advisory-lock timeout/cleanup and forced post-write rollback leave registry/output state unchanged.
- [x] Text/integer/UUID identity, nondeterministic collation rejection, permission-denial sanitisation and privilege-neutral predecessor assertions remain green.
- [x] Disposable PostgreSQL cleanup leaves no fixture roles, schemas or advisory locks.

## Documentation And Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'sec-linkage|sec-pseudonymise-postgres', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 PGHOST=127.0.0.1 PGPORT=55432 PGDATABASE=synthetic_records PGUSER=$USER scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'sec-linkage|sec-pseudonymise-postgres', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/check-local.sh
scripts/check-cran.sh
scripts/check-workflow-state.sh
git diff --check
```
