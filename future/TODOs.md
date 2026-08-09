# TODOs

This is the only repository task backlog. Roadmap issue
[#204](https://github.com/antoniojbt/episcout/issues/204) is the durable place
to return for the approved sequence.

## Working rules

- Keep one numbered package-code specification active at a time.
- Promote multi-step or consequential work to a numbered SDD/TDD specification
  before changing package code.
- Record baseline checks before implementation and reconcile this file,
  acceptance evidence and `changelog.md` whenever status changes.
- Keep draft/active specs directly under `future/specs/`; move accepted specs to
  `future/specs/done/`.
- Edit roxygen sources rather than generated `man/` files.
- Keep fixture expectations independently justified and routine tests offline.
- Never record an old or replacement Codecov credential.

## Current sequence

### 1. Narrow PostgreSQL EDA row-count reuse

- [ ] Complete issue
  [#220](https://github.com/antoniojbt/episcout/issues/220) through spec
  `022-postgresql-eda-row-count-reuse`:
  - [x] promote the existing scratch investigation and record the current
    PostgreSQL query-count baseline before code;
  - [x] reuse the existing transaction-local relation row count in categorical and
    binary summaries;
  - [x] remove exactly one redundant query per affected variable;
  - [x] do not broaden snapshot, privacy, bundle or reconciliation behaviour.

### 2. CURP validation and reconciliation planning

- [ ] After #220, plan issue
  [#217](https://github.com/antoniojbt/episcout/issues/217) as candidate spec
  `025-curp-validation-and-reconciliation`:
  - base validity, field derivation and check-digit behaviour on current
    authoritative sources rather than the missing photo reference;
  - replace the hard-coded birth-century cutoff with a reviewed deterministic
    rule and define vector, missing and invalid-input behaviour;
  - separate parsing from validation and from comparisons against collected
    demographic fields;
  - define how direct identifiers and row-level mismatches remain restricted.

## Parallel owner action

- [ ] Finish security issue
  [#213](https://github.com/antoniojbt/episcout/issues/213):
  - revoke/rotate the exposed Codecov credential as appropriate;
  - update or remove `CODECOV_TOKEN` according to the chosen upload policy;
  - request eligible GitHub cache/hidden-ref cleanup;
  - record completion without disclosing either credential.

Rewritten branches and historical tags are already upstream, and a
protected-`master` coverage upload passed for release commit `40ef702`.

## Deferred

- [ ] CRAN readiness/submission under
  [#81](https://github.com/antoniojbt/episcout/issues/81). The GitHub release is
  complete; no CRAN submission is currently authorised.
- [ ] PostgreSQL aggregate-bundle HTML rendering under
  [#196](https://github.com/antoniojbt/episcout/issues/196) and future spec
  `018-database-eda-report-rendering`.
- [ ] Clarify issue [#212](https://github.com/antoniojbt/episcout/issues/212)
  before treating “code complexity” as implementable work.
- [ ] Resolve the scope questions in issues
  [#61](https://github.com/antoniojbt/episcout/issues/61),
  [#62](https://github.com/antoniojbt/episcout/issues/62) and
  [#65](https://github.com/antoniojbt/episcout/issues/65) before creating specs.
- [ ] Define explicit contracts before adding cross-tool dictionary
  interoperability, database loading/connection helpers, visual-regression
  redesign or biomedical extensions.
- [ ] Implement completed design-only spec `026-epi-geo-series-plan` only through separately approved issues/specs for the phase-A vector file/in-memory foundation, phase-B read-only PostGIS source and bounded collection, and phase-C aggregate-only EDA coordinate integration. Inferential spatial analysis, raster and web mapping remain outside that programme.

## Done

### 2026-08-09

- [x] Complete issue #218's mapping-only `epi_geo_*` architecture as design-only spec `026-epi-geo-series-plan`, defining a GeoPackage/Shapefile primer, explicit CRS and coordinate-pair contracts, static `sf`/`ggplot2` mapping, a read-only bounded PostGIS path and aggregate-only EDA integration without changing package behaviour or the current roadmap sequence.

### 2026-08-08

- [x] Complete issue #215 through merged PR #219, accept spec
  `021-postgresql-identity-universe`, and move its record to
  `future/specs/done/` after all required checks passed.
- [x] Merge the post-release documentation and planning reconciliation through PR #216, pin the released installation command, align the workspace and move accepted specs 023/024 to `future/specs/done/` without claiming the unperformed owner walkthrough.
- [x] Publish GitHub release `0.3.0` from commit `40ef702` after exact package
  build/check/install and installed-package smoke validation.
- [x] Transfer rewritten historical tags upstream and verify a successful
  protected-`master` Codecov upload.
- [x] Create issue #215 and update issues #204, #213 and #81 to the post-release
  sequence.

### 2026-08-07

- [x] Complete issues #208/#209 through specs 023/024 and merged PR #211.
- [x] Complete the release-readiness audit through PR #210.
- [x] Complete issues #197/#198 through specs 019/020 and PRs #207/#206.

Older completed work remains recorded in `future/changelog.md` and accepted
specifications under `future/specs/done/`.
