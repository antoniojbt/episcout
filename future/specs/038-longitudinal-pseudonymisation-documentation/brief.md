# Brief

Spec ID: `038-longitudinal-pseudonymisation-documentation`
Status: Active

## Problem

The longitudinal pseudonymisation vignette still describes itself as a governance guide and contains approval, audit-first, review and restricted-schema wording that can imply package authority over processing, sharing or publication. The final `epi_sec_*` interfaces instead perform caller-selected technical operations, validate exact metadata and database invariants, and report neutral technical outcomes without granting or withholding permission.

## Objective

Reconcile the complete longitudinal guide and only verified directly related help with observable final `epi_sec_*` formals, classes, component/result schemas, statuses, duplicate rules, stable-token scope, registry lifecycle, transaction ownership, locking, rollback, recovery and EDA handoff. Preserve the factual boundaries that pseudonymisation is reversible and is not anonymisation or automatic disclosure control, and that credentials, PostgreSQL permissions and operational controls remain external responsibilities.

## Observable Outcome

The guide explains how callers declare and request exact identity-universe, linkage, registry and pseudonymisation operations; how the functions read, write, lock, commit, roll back and report; and how to retry or hand outputs to EDA. It does not present audit as mandatory, prescribe restricted schema names, infer approval from a result, or imply that episcout decides whether processing, use, sharing or publication may proceed.

## Authority And Baseline

The repository owner promoted `issue-269` as the terminal documentation slice after completed documentation predecessor `issue-268`/`PR-295` and completed behavioural evidence `issue-285`/`spec-037`/`PR-291` plus closeout `PR-292`. The isolated branch, fetched `origin/master` and canonical GitHub merge for `PR-295` all resolve to exact `commit-d79dd3c05ea1c64db2acd27b5b0af6b8193646b6`. Source, roxygen and focused tests at that base confirm seven retained `epi_sec_*` exports, neutral statuses and schemas, configured-role privilege behaviour, exact duplicate handling, transaction/lock/rollback integrity and value-free default diagnostics.

## Scope

- `vignettes/longitudinal-pseudonymisation.Rmd` as the primary deliverable.
- Directly related roxygen sources and generated Rd only if source/help comparison proves a mismatch.
- `spec-038` and directly required active-lifecycle records.
- Focused offline and disposable live PostgreSQL verification, vignette render inspection, generated-documentation consistency, the complete local check, workflow-state validation and diff review.

## Exclusions

- Any `epi_sec_*` API, database, cryptographic or behavioural change.
- Production registry operations, real identifiers, credentials, keys, private paths, bridge tables or record-level diagnostics.
- Broad onboarding, EDA or geospatial documentation changes beyond verified cross-link consistency.
- Release work, security-owner actions, new dependencies, orchestration, publication or GitHub mutation.

## Recovery

The implementation changes documentation and lifecycle records only. Every vignette code chunk that could touch PostgreSQL remains unevaluated during rendering. Rendered test output goes to a temporary directory, disposable PostgreSQL tests use runtime-named schemas with cleanup, and the scoped commit can be reverted without migrating or deleting package data or database objects.

## Terminal Disposition

This is the terminal planned documentation slice. No package-code successor is created automatically; post-merge closeout may use one focused terminal maintenance PR to record canonical merge evidence and archive this specification.
