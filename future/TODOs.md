# TODOs

This is the repository's synchronised task index. GitHub roadmap issue [#227](https://github.com/antoniojbt/episcout/issues/227) and its linked issues are authoritative for live state and sequence.

## Working rules

- Keep one numbered package-code specification active at a time.
- Run `scripts/check-workflow-state.sh` before new tracked work, at pull-request handoff and during post-merge closeout.
- Promote multi-step or consequential work to a numbered SDD/TDD specification before changing package code.
- Record baseline checks before implementation and reconcile this file, acceptance evidence and `changelog.md` whenever status changes.
- Keep draft, active and review specs directly under `future/specs/`; move completed specs to `future/specs/done/` only after canonical merge verification and closeout.
- Before closing planning-only work, create its next implementation issue or record an explicit terminal reason.
- Do not start a successor while the previous PR is merged but its closeout remains incomplete.
- Edit roxygen sources rather than generated `man/` files.
- Keep fixture expectations independently justified and routine tests offline.
- Never record an old or replacement Codecov credential.

## Current sequence

### 1. CURP structural audit implementation

- [ ] Complete issue [#225](https://github.com/antoniojbt/episcout/issues/225) through review spec `025-curp-validation-and-reconciliation` and draft PR [#231](https://github.com/antoniojbt/episcout/pull/231):
  - [x] accept the public audit-result and one-cycle legacy compatibility contract;
  - [x] reject silent normalisation and accept the explicit 1900–2099 local domain;
  - [x] pin the reviewed 2021 RENAPO birthplace catalogue and restricted synthetic-fixture provenance;
  - [x] defer checksum verification as `not_verified` to evidence issue [#230](https://github.com/antoniojbt/episcout/issues/230);
  - [x] change the spec from `draft` to `active` before package-code work;
  - [x] implement and locally verify the accepted vector, validation, derivation, comparison and privacy contracts;
  - [ ] reconcile acceptance and create the next tracker or terminal rationale before closure.

## Parallel owner action

- [ ] Finish security issue [#213](https://github.com/antoniojbt/episcout/issues/213):
  - revoke/rotate the exposed Codecov credential as appropriate;
  - update or remove `CODECOV_TOKEN` according to the chosen upload policy;
  - request eligible GitHub cache/hidden-ref cleanup;
  - record completion without disclosing either credential.

Rewritten branches and historical tags are already upstream, and a
protected-`master` coverage upload passed for release commit `40ef702`.

## Deferred

- [ ] Phase-A file and in-memory mapping under [#226](https://github.com/antoniojbt/episcout/issues/226), promoted from completed design-only spec `026-epi-geo-series-plan` only after roadmap approval.
- [ ] CRAN readiness/submission under [#81](https://github.com/antoniojbt/episcout/issues/81). The GitHub release is complete; no CRAN submission is currently authorised.
- [ ] PostgreSQL aggregate-bundle HTML rendering under [#196](https://github.com/antoniojbt/episcout/issues/196) and future spec `018-database-eda-report-rendering`.
- [ ] Clarify issue [#212](https://github.com/antoniojbt/episcout/issues/212) before treating “code complexity” as implementable work.
- [ ] Resolve the scope questions in issues [#61](https://github.com/antoniojbt/episcout/issues/61), [#62](https://github.com/antoniojbt/episcout/issues/62) and [#65](https://github.com/antoniojbt/episcout/issues/65) before creating specs.
- [ ] Define explicit contracts before adding cross-tool dictionary
  interoperability, database loading/connection helpers, visual-regression
  redesign or biomedical extensions.
- [ ] After #226 is promoted and completed, create only the next approved issue/spec for phase-B read-only PostGIS collection; retain phase-C aggregate-only EDA coordinate integration in spec 026 until then. Inferential spatial analysis, raster and web mapping remain outside that programme.

## Done

### 2026-08-09

- [x] Complete issue #220/spec 022 through merged PR #222 at `b07f9e8`; verify all required checks, reconcile acceptance and move the completed record under `future/specs/done/`.
- [x] Complete planning issue #217 through merged PR #224, retain spec 025 as a gated draft and create implementation successor #225.
- [x] Replace completed roadmap #204 with authoritative roadmap #227 and preserve deferred geo phase-A implementation in successor #226.
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
