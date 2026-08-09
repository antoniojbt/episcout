# Acceptance

Spec ID: `030-simplify-core-eda-controls-geo-outputs`
Status: Draft

## Planning And Activation

- [x] Live roadmap issue #227, canonical master and completed spec 029 were reconciled before new work.
- [x] Tracking issue #243 exists and roadmap #227 points to it.
- [x] The path-with-spaces workflow failure is reproduced independently for online and offline modes.
- [ ] The planning/prerequisite PR is green and its review gates are accepted before package implementation begins.

## Public And Schema Contracts

- [ ] The four runner signatures and documented defaults match the SDD.
- [ ] Lean scaffold, extended dictionary, catalogue, linkage and manifest schemas are exact.
- [ ] Deprecated schemas fail with actionable migration guidance.
- [ ] Intake-generated specifications are saved, returned and processed without approval states.
- [ ] Geo fields round-trip from both scaffold families into EDA specs without inference.

## Core And Map Behaviour

- [ ] Roles no longer suppress summaries, plots or stratification.
- [ ] Technical schema, missingness, preparation, identifier QA and coordinate QA remain correct.
- [ ] Data-frame map fixtures pass all boundaries, missingness, non-finite, empty, pair, theme and bound cases.
- [ ] PostgreSQL collection is requested-only, within-snapshot, inclusive-bounded and fail-not-truncate.
- [ ] Map objects, inventory rows, point counts, thematic values, IDs and paths reconcile independently.
- [ ] Core bundles publish only complete maps under `maps/` and retain atomic/checksum/overwrite guarantees.

## Security And Documentation

- [ ] Linkage column policy replaces combined dictionary policy without reducing security test coverage.
- [ ] Pseudonymisation output dictionaries/catalogues are semantic and EDA-ready.
- [ ] Specialised security manifests and restricted-data safeguards remain unchanged.
- [ ] Roxygen, README, NEWS, both EDA guides, geospatial primer, security guide, templates and database walkthrough are consistent.
- [ ] Migration documentation covers every immediate break and the replacement database-to-report workflow.

## Verification Evidence

- [ ] Focused offline suites pass.
- [ ] Live PostgreSQL 18 integration passes.
- [ ] Representative SVG and HTML artifacts are rendered and visually inspected.
- [ ] Changed R-file lint passes.
- [ ] `scripts/check-local.sh` passes.
- [ ] `scripts/check-cran.sh` passes with only explicitly reconciled external notes, if any.
- [ ] Online/offline workflow checks pass from normal and spaced paths.
- [ ] Required pull-request checks are green with no actionable review feedback.

## Closeout

- [ ] The implementation PR closes issue #243 only when every issue outcome is delivered.
- [ ] Canonical merge and issue closure are verified before moving this spec to `future/specs/done/`.
- [ ] Roadmap, TODO, changelog, acceptance evidence and local/fork master are reconciled.
- [ ] A successor is recorded or a terminal reason is added.
