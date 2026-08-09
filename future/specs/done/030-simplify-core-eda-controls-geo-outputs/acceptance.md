# Acceptance

Spec ID: `030-simplify-core-eda-controls-geo-outputs`
Status: Completed

## Planning And Activation

- [x] Live roadmap issue #227, canonical master and completed spec 029 were reconciled before new work.
- [x] Tracking issue #243 exists and roadmap #227 points to it.
- [x] The path-with-spaces workflow failure is reproduced independently for online and offline modes.
- [x] Draft planning/prerequisite PR #244 is fully green and its review gates are accepted before package implementation begins.

## Public And Schema Contracts

- [x] The four runner signatures and documented defaults match the SDD.
- [x] Lean scaffold, extended dictionary, catalogue, linkage and manifest schemas are exact.
- [x] Deprecated schemas fail with actionable migration guidance.
- [x] Intake-generated specifications are saved, returned and processed without approval states.
- [x] Geo fields round-trip from both scaffold families into EDA specs without inference.

## Core And Map Behaviour

- [x] Roles no longer suppress summaries, plots or stratification.
- [x] Technical schema, missingness, preparation, identifier QA and coordinate QA remain correct.
- [x] Data-frame map fixtures pass all boundaries, missingness, non-finite, empty, pair, theme and bound cases.
- [x] PostgreSQL collection is requested-only, within-snapshot, inclusive-bounded and fail-not-truncate.
- [x] Map objects, inventory rows, point counts, thematic values, IDs and paths reconcile independently.
- [x] Core bundles publish only complete maps under `maps/` and retain atomic/checksum/overwrite guarantees.

## Security And Documentation

- [x] Linkage column policy replaces combined dictionary policy without reducing security test coverage.
- [x] Pseudonymisation output dictionaries/catalogues are semantic and EDA-ready.
- [x] Specialised security manifests and restricted-data safeguards remain unchanged.
- [x] Roxygen, README, NEWS, both EDA guides, geospatial primer, security guide, templates and database walkthrough are consistent.
- [x] Migration documentation covers every immediate break and the replacement database-to-report workflow.

## Verification Evidence

- [x] Focused offline suites pass.
- [x] Live PostgreSQL 18 integration passes.
- [x] Representative SVG and HTML artifacts are rendered and visually inspected.
- [x] Changed R-file lint passes.
- [x] `scripts/check-local.sh` passes.
- [x] `scripts/check-cran.sh` passes with only explicitly reconciled external notes, if any.
- [x] Online/offline workflow checks pass from normal and fresh real spaced checkout paths at planning commit `1c056b8`.
- [x] Required pull-request checks are green with no actionable review feedback.

## Closeout

- [x] The implementation PR closes issue #243 only when every issue outcome is delivered.
- [x] Canonical merge and issue closure are verified before moving this spec to `future/specs/done/`.
- [x] Roadmap, TODO, changelog, acceptance evidence and local/fork master are reconciled.
- [x] A successor is recorded or a terminal reason is added.
