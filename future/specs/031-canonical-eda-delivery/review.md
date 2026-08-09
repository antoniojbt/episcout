# Review Notes

Spec ID: `031-canonical-eda-delivery`  
Status: Active

## Findings

- The current database run already owns all aggregate tables, plot/map inventories, SVGs, timings and a five-column checksum manifest, so a renderer does not need a PostgreSQL connection or row-level handoff.
- The current in-memory report template depends on R plot/map objects; the database template must instead consume validated CSVs and owned relative SVGs.
- Existing overwrite validation requires exact manifest ownership and sibling staging, providing the correct atomicity model for report publication.
- Database plot preparation is compact/aggregate. Map collection is feature-level and must not be published as plot data.
- Supporting a new layout behind an opt-in argument avoids breaking the current flat bundle and keeps renderer compatibility explicit.

## Open Questions

None. Issue #245 and owner direction settle HTML ownership, compatibility and governance boundaries.

## Planning Handoff

- Planning base: canonical `master` at `e037512a4474ca6653b0cd079341efffa0a12047`.
- Draft planning PR #251 passed PostgreSQL, Ubuntu, macOS, coverage, both
  Codecov gates and CodeFactor at `4b7b10b`; it has no actionable review
  feedback.
- Implementation is active on `agent/implement-canonical-eda-delivery`, stacked
  on that green planning head under the owner's instruction without waiting for
  merge.

## Closeout Notes

- Pull request and merge commit: pending.
- Required checks and material exceptions: pending.
- Tracking issue and roadmap disposition: #245 ready next under #249; #248 is successor.
