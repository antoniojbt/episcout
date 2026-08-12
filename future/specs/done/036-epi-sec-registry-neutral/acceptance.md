# Acceptance

Spec ID: `036-epi-sec-registry-neutral`
Status: Completed

## Activation

- [x] `issue-278`/`PR-281`/`spec-035` completed closeout on canonical `commit-e3bc0d5` before implementation began.
- [x] The predecessor diff, tests and accepted `spec-034` slice-2 contract were reconciled at that exact canonical baseline.
- [x] The owner-authorised stack-after-green promotion was recorded on `issue-284` before package edits.
- [x] The change remains bounded to registry and stable-path privilege neutrality; `issue-285` remains the explicit successor.

## Registry Contract

- [x] The registry export, public arguments/defaults, S3 class, physical schema/version and immutable token settings remain.
- [x] Registry results omit `schema_restricted`, use only `ready`, `initialisation_required` or structural `incompatible`, and print neutral technical text.
- [x] Registry inspection ignores ownership/grant state and classifies only relation kind, exact structure, metadata cardinality/version and settings.
- [x] Creation remains atomic and concurrent change rolls back without partial objects.

## PostgreSQL Privilege Neutrality

- [x] Registry, crosswalk and output paths perform no privilege query, grant or revoke.
- [x] Configured `PUBLIC` and named-role grants are unchanged before and after successful calls.
- [x] A compatible foreign-owned registry remains usable when PostgreSQL grants sufficient access.
- [x] Insufficient PostgreSQL permissions produce fixed value-free technical errors.

## Preserved Integrity

- [x] Source/registry/output separation, stable mappings, locks, rollback, row reconciliation and source non-mutation remain covered.
- [x] `existing = "replace"` remains limited to an owned ordinary non-partitioned dependency-free destination and never cascades.
- [x] No linkage, pseudonymisation-result, issue-severity, diagnostic-value or manifest contract assigned to `issue-285` changes.

## Documentation And Verification

- [x] Roxygen-generated help and only directly affected guide text match observed behaviour.
- [x] Focused offline and live PostgreSQL tests, package lint, the complete local check and `git diff --check` pass.
- [x] The software-verification, truth-and-semantics, copy-edit and render/release checklists were applied as self-review.
- [x] No real identifiers, production database, credentials, role-administration feature, unrelated refactor or new dependency enters the diff.
- [x] Final `PR-289` merged to canonical `master` as `commit-40b284f` on 2026-08-12. Hosted macOS/Ubuntu R CMD, PostgreSQL integration, test coverage, CodeQL and CodeFactor checks passed; Codecov reported one uncovered line while its informational status passed.
- [x] `scripts/check-workflow-state.sh` passes after canonical merge; issue-285 is the promoted successor and spec-036 is under `future/specs/done/`.
