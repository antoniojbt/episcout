# Review Notes

Spec ID: `037-epi-sec-linkage-results-neutral`
Status: Completed

## Baseline And Authority

The clean task branch, fetched `origin/master` and canonical `upstream/master` all matched `commit-6ba8f25213569aaffb55f8c10ef544a4800f6de6` at activation. `issue-284` completed through final `PR-289` at `commit-40b284f`; closeout `PR-290` made the completed `spec-036` record canonical at the activation head. `issue-285` and its owner comment explicitly authorise dispatcher execution and require `spec-037` before package edits.

## Predecessor Reconciliation

The merged predecessor diff removed privilege queries/refusals/revocations from registry, crosswalk and output handling, preserved configured grants and sanitised database failures, and strengthened live coverage for foreign ownership, permission denial, locking and rollback. It deliberately retained `validation_status`, `privacy_class`, `analytic_action`, `blocked`, `blocking`, `epi_sec_governance`, `epi_sec_blocked`, specialised diagnostic redaction and manifest `sensitivity`. Those remaining surfaces exactly match the bounded `issue-285` outcome and the third implementation boundary in completed `spec-034`.

## Baseline Verification

- `scripts/check-workflow-state.sh` passed online against `antoniojbt/episcout@master` before spec activation.
- The focused offline linkage/pseudonymisation selection passed; the three live PostgreSQL cases skipped only because `EPISCOUT_TEST_POSTGRES` was unset.
- No product, privacy, security or architecture judgement remains unresolved. The accepted contract fixes the new schemas, statuses, issue values, alias behaviour, manifest field and preservation boundary.

## Implementation Findings

- The linkage scaffold and validator now emit and consume only the accepted neutral schemas. Every action is explicit, exactly the declared identifier is pseudonymised and legacy `validation_status`/`privacy_class` values affect neither construction nor database execution.
- The one-cycle legacy constructor adapter covers `bridge`, `retain`, `retain_restricted` and `drop`, warns once, rejects non-executable actions and always returns the new schema. Database execution rejects saved or modified legacy-shaped objects with regeneration guidance.
- Completed audits now return `audit_complete`; error-severity apply findings and lock timeouts return `not_written`; committed applies alone return `complete`. Issues have only technical error/warning severities, the internal rollback carrier is `epi_sec_no_write`, and the former governance/blocker paths are absent.
- `include_issue_values` returns ordinary caller-selected data without a class, attribute or hiding method. Default results remain value-free, and the deprecated positional alias warns, maps both logical values and rejects an explicit conflict.
- Projection consumes only `output_action`, manifests use `output_type = "pseudonymised_table"`, and output dictionary/catalogue validation remains green. Generated help, the installed walkthrough and only directly affected longitudinal-guide sections were reconciled.
- Self-review found no unresolved correctness, data-integrity, privacy, security or compatibility defect in the scoped diff.

## Verification

- Focused offline linkage and PostgreSQL tests passed after the final code changes; the three explicitly live-gated cases skipped as designed.
- The complete focused suite passed against a disposable local PostgreSQL 18.4 server at `127.0.0.1:55432` using database `synthetic_records` and its bootstrap role. Cleanup checks returned zero `episcout_%` schemas, roles and advisory locks.
- Package-loaded `lintr::lint_package()` returned no findings.
- `scripts/check-local.sh` passed on the final files: the complete suite had 26 documented environment-gated skips, and its build and `R CMD check` completed with `0 errors`, `0 warnings` and `0 notes`.
- The changed longitudinal vignette rendered successfully to HTML, and plain-text inspection confirmed the neutral status, duplicate, lock-timeout and ordinary diagnostic wording.
- `scripts/check-cran.sh` completed under R 4.5.3 on Ubuntu 24.04 with `0 errors`, `0 warnings` and the inherited single incoming-feasibility NOTE: no prebuilt vignette index plus two unchanged Stack Overflow URLs returning HTTP 403. CRAN package-index access timed out, so the network-dependent portion of incoming checks was unavailable.
- The final pre-PR online `scripts/check-workflow-state.sh` passed and reported `issue-285`/`spec-037` as the sole active implementation against canonical `antoniojbt/episcout@master`; `git diff --check` passed on the task-scoped diff.
- Hosted macOS and Ubuntu package checks, PostgreSQL integration, test coverage, CodeQL, CodeFactor and both informational Codecov statuses passed at final `commit-5550b66`. CodeFactor reported no finding to record in `issue-288`.

## Checklist Self-review

- Software verification: callers, schemas, positional compatibility, generated declarations, database effects and rollback/error paths were traced; focused offline/live tests, lint and complete package checks exercise the changed interfaces.
- Truth and semantics: caller action and diagnostic choices are explicit; missing identifiers remain missing/error outcomes; aggregate counts, no-write status and committed-write status are not conflated; no classification or disclosure conclusion is inferred.
- Copy edit: directly affected help, examples and guide passages match observed behaviour, preserve British English and do not hard-wrap prose.
- Render and release: the changed vignette rendered and was inspected; source build and `R CMD check --as-cran` rebuilt the vignette and manuals successfully. No release or publication was performed.

This review is a self-review, not an independent implementation review.

## Lifecycle

- Tracking issue: `issue-285` under owner roadmaps `issue-274` and `issue-275`.
- Accepted design: completed `spec-034-retained-epi-sec-technical-contract`.
- Predecessor and closeout: `issue-284`, `PR-289` and `PR-290`.
- Working branch: `agent/issue-285-refactor-make-epi-sec-linkage-results-issues` from canonical `commit-6ba8f25`.
- Downstream documentation tracker: `issue-269`.
- `PR-291` merged to canonical `master` as `commit-0d9b302` on 2026-08-12 and closed `issue-285`.
- Post-merge closeout verified the canonical merge commit and final hosted checks, completed and archived this specification, and left `issue-269` waiting only for its independent `issue-268` documentation-lane dependency.
