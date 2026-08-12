# Review Notes

Spec ID: `036-epi-sec-registry-neutral`
Status: Completed

## Baseline

Canonical `master` contains completed `spec-035` through PR-283. The workflow guard passes, and issue-284 is promoted with the owner-recorded stack-after-green authority. No package implementation specification is otherwise active or in review.

## Implementation review

Registry inspection now reads only relation names and kinds, exact physical structure/version and immutable token settings. It no longer reads ownership or PUBLIC privileges. Apply retains its repeatable-read transaction and inside-transaction empty-state recheck, but creates no grant or revocation statement. The registry result and print method remove `schema_restricted`; incompatible existing objects return `incompatible` in audit mode.

The live PostgreSQL regression grants PUBLIC CREATE/USAGE on a disposable registry schema, records those grants, initialises the registry, verifies unchanged grants and removes only test objects before the later pseudonymisation scenarios. It also verifies that a publicly readable registry table does not change registry audit compatibility.

Focused offline tests passed with expected live gates skipped. Focused live PostgreSQL tests passed against a disposable PostgreSQL 17/PostGIS container. Package lint, workflow-state and diff checks passed. The complete local test command was started but its command transport did not return a final completion result, so full local-check status is not claimed.

## Closeout

PR-286 merged to canonical `master` as `commit-6312b8d` on 2026-08-12. Hosted macOS/Ubuntu R CMD checks, PostgreSQL integration, test coverage and Codecov statuses passed. CodeFactor failed; the owner has classified it as informational, so it does not block merge, closeout or authorised successor stacking. issue-285 remains the staged successor; the completed record moves under `future/specs/done/`.
