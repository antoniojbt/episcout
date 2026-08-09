# Review Notes

Spec ID: `029-eda-reviewed-coordinate-roles`
Status: Draft

## Findings

- The completed Phase-C design in spec 026 and issue #237 agree on three optional reviewed fields and an aggregate-only QA component.
- The current EDA stack has separate data-frame and PostgreSQL paths plus three orchestration/publication surfaces; the implementation must reconcile all of them rather than creating an isolated result that bundles omit.
- EPSG:4326 has a defensible package-wide reviewed range contract from Phase A. Other resolvable CRSs do not have one generic safe numeric range, so their `range_failures` count remains zero instead of inventing area-of-use rules.
- Existing missing-code semantics must apply to pair QA before finite/range classification to preserve specification-first behaviour.

## Open Questions

None. The issue contract and completed source designs resolve the implementation boundary.

## Planning Handoff

- Planning PR: draft PR #240; required checks are pending.
- Implementation base: the green planning PR head, under the owner's stacking instruction.
- No package source, test, dependency, namespace, generated help or public behaviour changes belong in the planning PR.

## Closeout Notes

- Implementation PR and merge commit: pending.
- Required checks and material exceptions: pending.
- Tracking issue and roadmap disposition: pending.
- Terminal reason: bounded Phase-C completes the current geospatial programme; future inference requires a separate concrete scientific tracker.
