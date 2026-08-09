# Review Notes

Spec ID: `029-eda-reviewed-coordinate-roles`
Status: Review

## Findings

- The completed Phase-C design in spec 026 and issue #237 agree on three optional reviewed fields and an aggregate-only QA component.
- The current EDA stack has separate data-frame and PostgreSQL paths plus three orchestration/publication surfaces; the implementation must reconcile all of them rather than creating an isolated result that bundles omit.
- EPSG:4326 has a defensible package-wide reviewed range contract from Phase A. Other resolvable CRSs do not have one generic safe numeric range, so their `range_failures` count remains zero instead of inventing area-of-use rules.
- Existing missing-code semantics must apply to pair QA before finite/range classification to preserve specification-first behaviour.

## Open Questions

None. The issue contract and completed source designs resolve the implementation boundary.

## Planning Handoff

- Planning PR #240 passed macOS, Ubuntu, PostgreSQL integration, coverage, both Codecov gates and CodeFactor at `efd4b2d`.
- Implementation base: the green planning PR head, under the owner's stacking instruction.
- No package source, test, dependency, namespace, generated help or public behaviour changes belong in the planning PR.

## Implementation Evidence

- Data-frame tests cover legacy/blank specifications, validation failures, normalized and ordered pairs, independently stated missing/non-finite/range counts, sentinels, zero rows, value-free failures, workflow integration and byte-level return/bundle privacy.
- Disposable PostgreSQL 18.4 tests passed on an ordinary table with non-syntactic identifiers. Direct profiling and database bundle publication returned one aggregate row per pair, did not require PostGIS, and left the caller-owned connection open and idle after failure.
- `scripts/check-local.sh` passed with 0 errors, 0 warnings and 0 notes; the complete offline test suite and package lint passed within it.
- `scripts/check-cran.sh` passed with no errors or warnings and the one recorded incoming-feasibility NOTE for a new submission, its absent prebuilt vignette index and two existing Stack Overflow 403 responses.
- Focused data-frame and live PostgreSQL geo suites, `scripts/check-workflow-state.sh` and `git diff --check` passed after the final reconciliation hardening.
- Ordinary summaries and plots policy-skip reviewed coordinate variables; serialized returns and every bundle artifact were checked for distinctive coordinate tokens and geometry markers.
- Pull-request head `2a668d4` passed PostgreSQL integration, Ubuntu, macOS, coverage, Codecov project/patch and CodeFactor checks.

## Closeout Notes

- Implementation PR: #241; merge commit pending.
- Required local and pull-request checks: passed at `2a668d4`; final evidence-only head checks pending.
- Tracking issue and roadmap disposition: pending.
- Terminal reason: bounded Phase-C completes the current geospatial programme; future inference requires a separate concrete scientific tracker.
