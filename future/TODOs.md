# TODOs

This is the repository's synchronised task index. GitHub roadmap issue [#249](https://github.com/antoniojbt/episcout/issues/249) and its linked issues are authoritative for live state and sequence.

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

Issue [#245](https://github.com/antoniojbt/episcout/issues/245) is active spec `031-canonical-eda-delivery`. Planning PR #251 is green at `4b7b10b`; the stacked implementation now has green focused/offline/live PostgreSQL tests, package lint, local and CRAN-oriented checks, an installed walkthrough and exact artifact inspection. Draft implementation-PR handoff is next. [#248](https://github.com/antoniojbt/episcout/issues/248) follows #245 with explicit counts, denominators, percentages and subgroup sizes.

## Deferred owner action

- [ ] Finish security issue [#213](https://github.com/antoniojbt/episcout/issues/213):
  - revoke/rotate the exposed Codecov credential as appropriate;
  - update or remove `CODECOV_TOKEN` according to the chosen upload policy;
  - request eligible GitHub cache/hidden-ref cleanup;
  - record completion without disclosing either credential.

Rewritten branches and historical tags are already upstream, and a
protected-`master` coverage upload passed for release commit `40ef702`.

## Deferred

- [ ] CRAN readiness/submission under [#81](https://github.com/antoniojbt/episcout/issues/81). The GitHub release is complete; no CRAN submission is currently authorised.
- [ ] Clarify issue [#212](https://github.com/antoniojbt/episcout/issues/212) before treating “code complexity” as implementable work.
- [ ] Resolve the scope questions in issues [#61](https://github.com/antoniojbt/episcout/issues/61), [#62](https://github.com/antoniojbt/episcout/issues/62) and [#65](https://github.com/antoniojbt/episcout/issues/65) before creating specs.
- [ ] Define explicit contracts before adding cross-tool dictionary
  interoperability, database loading/connection helpers, visual-regression
  redesign or biomedical extensions.

## Done

### 2026-08-09

- [x] Replace completed roadmap #227 with authoritative roadmap #249. Promote issue #245 as ready next, place issue #248 directly after it, defer #213/#81/#61/#62/#65/#212, close #196 as superseded by #245 and close #235 as not planned. The #245 contract makes HTML the normal human entry point for validated aggregate bundles while CSV/SVG/manifests remain canonical evidence and no governance decision is added.
- [x] Complete issue #243/spec 030 through planning PR #244 (`2d2237b`) and implementation PR #246, merged to canonical `master` as `825215ea0eb2c7aab79768d6174315f1708bec09`; issue #243 closed automatically, PostgreSQL integration, Ubuntu, macOS, coverage, both Codecov gates and CodeFactor passed, and the completed terminal record moved to `future/specs/done/`. No successor was created because the remaining roadmap work is separately tracked or deferred.
- [x] Complete issue #237/spec 029 through planning PR #240 (`d97d8bf`) and implementation PR #241, merged to canonical `master` as `308d5442b9c7d0e180343c89f97e9f0fb8668062`; issue #237 closed automatically, PostgreSQL integration, Ubuntu, macOS, coverage, both Codecov gates and CodeFactor passed, and the completed terminal record moved to `future/specs/done/`. No successor was created: later spatial inference requires a new concrete scientific tracker.
- [x] Complete issue #233/spec 028 through PR #238, merged to canonical `master` as `460acd0908044e963827ddc4d2a865984ed9fc76`; issue #233 closed automatically, macOS, Ubuntu, PostgreSQL integration, coverage, both Codecov gates and CodeFactor passed, Codecov reported 96.31% patch coverage and project coverage increased from 92.25% to 92.43%, the completed record moved to `future/specs/done/`, and issue #237 is the ready-next tracker after this closeout becomes canonical.
- [x] Complete issue #226/spec 027 through PR #234, merged to canonical `master` as `b37b391`; issue #226 closed automatically, all required CI passed, Codecov reported every modified coverable line tested and project coverage at 92.25%, the completed record moved to `future/specs/done/`, and issue #233 is the ready-next tracker after this closeout becomes canonical.
- [x] Complete issue #225/spec 025 through PR #231, merged to canonical `master` as `7e42f22`; issue #225 closed automatically, all required CI passed, the completed record moved to `future/specs/done/`, and issue #226 is the approved next tracker. Checksum remains `not_verified`; owner-resolved issue #230 closed without adopting an unofficial algorithm.
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
