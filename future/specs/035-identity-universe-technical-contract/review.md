# Review Notes

Spec ID: `035-identity-universe-technical-contract`
Status: Active

## Pre-implementation Findings

- The current constructor centralises the six-column confirmation gate and fingerprints version 1, so the five-column schema, legacy adapter and version check can be changed locally without altering linkage metadata helpers.
- Audit and materialisation already share the same aggregate audit path; changing audit status independently from issue severity does not require changing set calculations.
- Expected materialisation no-commit outcomes are concentrated in one error-severity predicate, destination issue and lock issue. The internal rollback carrier can be renamed without adding a public condition.
- Identity-universe privilege coupling is limited to one output-schema `PUBLIC` query and one created-table `REVOKE`; shared helpers must remain because later registry/pseudonymisation slices still use them.
- The current live fixture independently supports all required union and overlap expectations. Its rollback trigger must move from the removed revoke statement to a failure after the insert has executed.

## Semantic Decisions

- `validation_status` is ignored before character normalisation, so missing, blank and arbitrary legacy values cannot affect construction.
- Audit completion describes a completed technical inspection, not absence of error findings. Error findings affect only requested writes.
- Null, blank, invalid and collision counts retain their technical meaning; only policy-bearing names change.
- PostgreSQL privilege configuration is neither copied into ordinary results nor interpreted by the package. Permission denial remains a sanitised server error.

## Open Questions

None. `Issue-278` and completed `spec-034` settle the bounded interface, compatibility, transaction and successor boundaries.

## Baseline Evidence

The branch and local `master` initially matched fetched `origin/master` at `commit-cc05cb0`; `PR-277` was merged, `issue-276` was closed and `issue-278` was open. The initial workflow guard was structurally consistent but exited `2` because the disposable clone lacked the required `upstream` remote. Before handoff, the same canonical URL was added locally as `upstream`, the task branch fast-forwarded over the instructions-only `commit-afe109b`, and the online guard passed against `upstream/master`.

## Implementation Review

The constructor returns only the exact five-column version-2 schema and fingerprints the deterministic contract. The legacy adapter discards `validation_status` before normalisation, emits one warning and produces an object identical to direct five-column construction. Strict reconstruction rejects version-1 objects, modified shapes, changed classes and changed fingerprints without repairing them.

Audit and write decisions now separate completion from findings: audit returns `audit_complete` after a completed snapshot, while only error-severity findings prevent materialisation. Source/namespace status, issue severity, existing-destination and lock-timeout paths use neutral enums, and the internal rollback carrier is caught before an ordinary no-write result is returned.

The identity-universe path no longer calls either shared privilege helper and emits no grant/revoke SQL. The shared helpers and every later registry/linkage/pseudonymisation caller are unchanged. Source validation, exact union/overlap SQL, constraints, timeouts, advisory-lock transfer, inside-transaction revalidation and database-boundary sanitisation remain localised to the same paths.

## Independent Calculation And Live Evidence

The generated two-source fixture retained the hand-derived results: distinct counts `3/3`, duplicate excess `1/0`, union `4`, single-source `2`, multi-source `2`, intersection `2`, exclusive counts `1/1` and directional coverage `2/3` in both directions. Null, blank and regex-invalid inputs returned error-severity counts `1/1/1`; the empty source returned one warning and zero-denominator coverage remained `NA`.

The PostgreSQL 18.4 test created disposable schemas and synthetic values only. Explicit `PUBLIC` `CREATE`/`USAGE` schema privileges were true before and after successful materialisation, captured identity-universe SQL contained no privilege query, `GRANT` or `REVOKE`, and the connected role's permissions determined success. A mocked failure after the destination insert had executed produced only the fixed rollback error and left no destination table.

## Documentation Review

Roxygen regenerated only the two intended identity-universe Rd contracts after unrelated generator drift was restored. The source formals, exact five source columns, version, status/severity values, transaction behaviour and privilege neutrality match the generated help. The longitudinal vignette rendered to a temporary 69,151-byte HTML artifact; the complete bounded section was inspected as plain rendered text and contained the version-2 fields, optional inspection, `audit_complete`/`not_written` semantics and no stale identity-universe confirmation or privilege-mutation claim. The temporary artifact was not retained or published.

## Verification

- Focused offline identity-universe selection: all enabled expectations passed; the live file skipped only because `EPISCOUT_TEST_POSTGRES` was intentionally unset in that invocation.
- Focused live identity-universe selection: all expectations passed against disposable PostgreSQL 18.4 at `127.0.0.1:55432` with generated schemas and values.
- Package-loaded lint: no findings.
- `scripts/check-local.sh`: documentation, lint, full tests, build and R CMD check passed with `0 errors`, `0 warnings` and `0 notes`; 25 environment-gated test cases skipped as reported, including the separately verified identity-universe live case.
- `scripts/check-workflow-state.sh`: passed online with `spec-035` as the sole active implementation and canonical `upstream/master` at `commit-afe109b`.
- `git diff --check`: passed.

The software-verification, truth-and-semantics, analysis-and-statistics, copy-edit and render/release checklists were applied as a self-review, not an independent review. No coverage percentage was measured. No unresolved correctness or documentation finding remains.

## Lifecycle Handoff

`Issue-278`/`spec-035` remains `active` because this dispatch explicitly excludes GitHub and pull-request mutation. The local implementation is committed and clean; pull-request review, merge, post-merge closeout and creation of the second privilege-neutrality tracker remain pending. The successor must not activate before this slice's canonical closeout passes `scripts/check-workflow-state.sh`.
