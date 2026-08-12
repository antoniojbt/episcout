# Acceptance

Spec ID: `035-identity-universe-technical-contract`
Status: Active

## Activation

- [x] `PR-277` is merged, `issue-276`/`spec-034` is completed and closeout is canonical at baseline `commit-cc05cb0`.
- [x] `Issue-278` is the sole active implementation tracker and the exact behaviour contract is resolved before package edits.
- [x] The change is bounded to the identity-universe slice; no later `epi_sec_*` refactor is active.

## Version-2 Specification

- [x] Both exports, public arguments/defaults and result/specification classes remain.
- [x] New specifications contain exactly five metadata-only source columns, `identity-universe-2` and a deterministic SHA-256 fingerprint.
- [x] The exact legacy six-column constructor form warns once, ignores every `validation_status` value and returns the same version-2 contract.
- [x] Arbitrary extra/value-bearing fields remain errors, and saved version-1 or modified objects fail with regeneration guidance.
- [x] Print and help text contain no confirmation, approval or mandatory-audit claim.

## Audit And Materialisation

- [x] Audit always returns `audit_complete`, writes false and warning/error issues after a completed inspection.
- [x] Source/namespace statuses use only `ready`, `warning`, `error`; issue severity uses only `warning`, `error`.
- [x] Materialisation returns `not_written` for technical error findings, destination existence and lock timeout, and `complete` only after commit.
- [x] Aggregate schemas, exact typed identity, null/blank/regex/collision/duplicate/empty handling and value-free ordinary results remain intact.
- [x] The independent `{A, B, B, C}`/`{B, C, D}` fixture retains all required source, union, membership, intersection and directional-coverage results.

## PostgreSQL Integrity

- [x] Source relation/type/collation, schema/destination, transaction ownership, statement timeout, locks, uniqueness/check constraints, source non-mutation and fixed database errors remain covered.
- [x] The identity-universe path performs no privilege query, grant or revoke and succeeds in an explicitly public disposable schema when the connected role has permission.
- [x] Before/after schema grant state is identical.
- [x] A forced post-insert non-permission failure removes the destination completely through rollback.

## Documentation And Verification

- [x] Roxygen-generated Rd matches the implementation, and only the bounded guide section changes.
- [x] Focused offline and live PostgreSQL tests, lint, `scripts/check-local.sh`, `scripts/check-workflow-state.sh` and `git diff --check` pass or exact environment-dependent skips are recorded.
- [x] The software-verification, truth-and-semantics, analysis-and-statistics, copy-edit and render/release checklists are applied as self-review.
- [x] No real identifiers, production database, credentials, governance decision, unrelated refactor or new dependency enters the diff.
- [x] The task commit is focused and the worktree is clean; pull request, merge and post-merge closeout remain outside this dispatch.
