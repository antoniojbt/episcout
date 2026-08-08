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

### 1. Multi-table PostgreSQL identifier universe

- [ ] Complete issue
  [#215](https://github.com/antoniojbt/episcout/issues/215) through spec
  `021-postgresql-identity-universe`:
  - create the spec and record the package/PostgreSQL baseline before code;
  - implement an audit-first, PostgreSQL-resident, value-free-by-default
    identifier-universe workflow;
  - materialise only an explicitly confirmed blocker-free restricted universe;
  - preserve existing linkage, registry and pseudonymisation behaviour;
  - use synthetic unit/live PostgreSQL evidence for success, blocking,
    redaction, rollback, timeout and lock behaviour.

### 2. Narrow PostgreSQL EDA row-count reuse

- [ ] After #215, promote
  `future/scratch/episcout_postgres_eda_performance_issue.md` to a dedicated
  issue and spec `022-postgresql-eda-row-count-reuse`:
  - reuse the existing transaction-local relation row count in categorical and
    binary summaries;
  - remove exactly one redundant query per affected variable;
  - do not broaden snapshot, privacy, bundle or reconciliation behaviour.

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

## Done

### 2026-08-08

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
