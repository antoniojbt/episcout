# Review Notes

Spec ID: `036-epi-sec-registry-neutral`
Status: Review

## Baseline

The task branch, fetched `origin/master` and canonical `upstream/master` matched `commit-e3bc0d5` at activation. `PR-281` merged as `commit-ebd8d35`, and closeout `PR-283` made predecessor reconciliation canonical. The owner recorded stack-after-green promotion on `issue-284`; the predecessor diff, tests and completed `spec-034` slice-2 contract were reconciled before package edits.

## Implementation Review

Registry inspection now reads only relation names and kinds, exact physical structure/version and immutable token settings. It no longer reads ownership or `PUBLIC` privileges. Apply retains its repeatable-read transaction and inside-transaction empty-state recheck, but creates no grant or revocation statement. The registry result and print method remove `schema_restricted`; incompatible existing objects return `incompatible` in audit mode.

The stable registry path in `epi_sec_pseudonymise_db()` no longer tests registry/output schemas or crosswalk objects for `PUBLIC` access and no longer revokes output-table privileges. Registry and pseudonymisation failures use fixed technical messages. Source/registry/output separation, registry reinspection, advisory locks, atomic registry/output writes, source non-mutation and post-write reconciliation remain unchanged. Destructive output replacement still requires an owned ordinary non-partitioned dependency-free destination and never uses `CASCADE`.

Offline tests independently capture registry catalogue and creation SQL, assert the exact seven-field result, immutable settings and destructive-target boundary, and verify fixed database errors. The live PostgreSQL fixture records package-issued SQL, preserves `PUBLIC` and complete named-role ACL state, uses a compatible foreign-owned registry under sufficient server permissions, exercises a permission denial behind a fixed value-free error and forces failure after output creation to prove complete rollback.

## Concurrent Canonical Change

During this isolated run, `PR-286` merged as canonical `commit-6312b8d` and GitHub closed `issue-284`. Read-only reconciliation confirmed that the merged diff removed privilege coupling only from `epi_sec_identity_registry_init()`; `R/epi_sec_pseudonymise_db.R` still contained the issue's prohibited registry/output schema checks, crosswalk checks and automatic output revocation. The task commit was therefore rebased onto `commit-6312b8d` and retained only the complete bounded corrective diff. No GitHub state was changed.

## Verification

- Focused offline registry/pseudonymisation tests pass; only the three explicit live PostgreSQL cases skip when their environment gate is unset.
- All three focused live cases pass against a disposable local PostgreSQL 18.4 database, including grant neutrality, foreign ownership, permission denial, locking and rollback; cleanup leaves zero disposable roles and schemas.
- Package-loaded lint reports no findings.
- The longitudinal vignette renders successfully, and plain-text inspection confirms the connected-role permission boundary, `initialisation_required`, `incompatible` and sanitised-error guidance.
- `scripts/check-local.sh` passes the complete suite and package check with 0 errors, 0 warnings and 0 notes; its 26 skips are the repository's explicit environment-gated PostgreSQL/PostGIS, installed-package parallel and visual tests. Generated unrelated Rd drift and skipped visual snapshots were restored after the check.
- `git diff --check` passes.
- `scripts/check-workflow-state.sh` reports the expected lifecycle blocker: GitHub has closed `issue-284` and merged `PR-286` while the complete corrective diff remains unpublished and `spec-036` is still in review.

## Checklist Self-review

The software-verification checklist found no untested changed branch, dependency, generated-interface drift or unexplained broad-suite failure. The truth-and-semantics review confirms that package results do not reinterpret PostgreSQL privilege state and that configured ACLs are compared independently before and after calls. The copy-edit review retains British spelling, neutral technical language and the direct privacy boundary without broad guide rewrites. The render/release review inspected the rendered guide and package build; no release artifact or publication is authorised.

## Remaining Lifecycle Action

The implementation is complete and committed locally. A repository owner must reopen or otherwise reconcile the prematurely closed tracker, publish the corrective commit through an authorised pull request, rerun hosted checks and complete canonical closeout before `issue-285` starts. Those GitHub mutations are outside this disposable-clone task.
