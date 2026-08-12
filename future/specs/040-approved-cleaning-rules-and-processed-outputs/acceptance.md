# Acceptance

Spec ID: `040-approved-cleaning-rules-and-processed-outputs`

Status: Active

Tracking issue: issue-272

## Preconditions And Boundary

- [x] Canonical `upstream/master` is commit-962444a after completed issue-271/spec-039/PR-299 and closeout PR-300.
- [x] Issue-272 and roadmap issue-249 authorise this bounded implementation, and issue-273 remains staged behind it.
- [x] The exact approved-rule schema, opaque approval provenance, pending-proposal rejection and descriptive dictionary boundary are recorded before package source changes.

## Implementation

- [x] Exact approved rules reject pending, malformed, contradictory and unsupported input before processing.
- [x] Numeric bounds, categorical/binary allowed values and approved missing codes create stable typed missing values.
- [x] In-memory processing preserves its source, row count, row order, row names and column order and returns the complete processed data frame.
- [x] Explicit CSV/RDS publication refuses replacement and leaves no partial artifact on failure.
- [x] PostgreSQL creates only a new user-named table through server-side transformation in one transaction and rolls back on any failure.
- [x] Aggregate audit hashes rules, records source/destination dimensions and before/after/transition counts and reconciles before success without disclosing private values or identity.
- [x] Zero rows, all-missing input, collisions, rollback and equivalent supported in-memory/PostgreSQL behaviour are tested.
- [x] Roxygen help, README, NEWS, vignette and project/workflow records agree with observed behaviour.

## Verification And Handoff

- [x] Focused non-database tests pass.
- [x] Focused disposable PostgreSQL integration tests pass.
- [x] Changed R source is styled and package lint passes.
- [x] `scripts/check-workflow-state.sh` passes before handoff.
- [x] `scripts/check-local.sh` passes with zero errors and warnings; the environment retains only the documented current-time verification NOTE.
- [x] Final diff, privacy canaries, generated files and staged commit are reviewed.
- [ ] A future pull request records issue-272, spec-040, verification, compatibility, issue-273 disposition and post-merge closeout ownership.
- [ ] Canonical merge and issue closure are verified before setting this manifest to `completed` and moving spec-040 under `future/specs/done/`.
