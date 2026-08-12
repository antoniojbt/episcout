# Acceptance

Spec ID: `041-reviewed-civil-date-derivation`

Status: Completed

Tracking issue: issue-273

## Preconditions And Boundary

- [x] Canonical `upstream/master` is commit-3254bc8 after completed issue-272/spec-040/PR-301 and closeout PR-302.
- [x] Issue-273 and roadmap issue-249 authorise this bounded terminal implementation.
- [x] The exact approved-operation, explicit civil-date declaration, timezone-free storage and atomic publication boundaries are recorded before package source changes.

## Implementation

- [x] Exact approved operations reject inferred, pending, malformed, unsafe or mutated input before processing.
- [x] In-memory strict local timestamps and PostgreSQL `timestamp without time zone` produce a separate `Date`/`date` column without changing the source.
- [x] Every non-missing source value must be exact midnight; aggregate-only failure blocks all columns and publications.
- [x] Missing values, calendar boundaries, fractional seconds, zero rows and destination collisions behave deterministically.
- [x] Explicit CSV/RDS publication refuses replacement and leaves no partial artefact on failure.
- [x] PostgreSQL creates only a new table in one transaction, collects no timestamps and rolls back on any failure.
- [x] Roxygen help, README, NEWS, vignette and project/workflow records agree with observed behaviour.

## Verification And Handoff

- [x] Focused non-database tests pass with only the expected PostgreSQL-gate skip.
- [x] Focused disposable PostgreSQL 18.4 integration tests pass without skips.
- [x] Changed R source is styled and package lint passes.
- [x] The changed vignette renders and its civil-date section and literal output are inspected.
- [x] `scripts/check-workflow-state.sh` passes before handoff.
- [x] `scripts/check-local.sh` passes with zero errors and warnings; only the environmental current-time NOTE remains.
- [x] Final diff, privacy canaries, generated files and staged commit are reviewed.
- [x] The task-scoped implementation is committed and the worktree is clean at dispatch handoff.
- [x] PR-303 records issue-273, spec-041, verification, compatibility, terminal disposition and post-merge closeout ownership.
- [x] PR-303 merged to canonical `master` as commit-db839d0, issue-273 closed, and spec-041 is completed under `future/specs/done/` with no successor.
