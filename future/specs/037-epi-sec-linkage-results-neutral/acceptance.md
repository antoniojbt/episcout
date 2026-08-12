# Acceptance

Spec ID: `037-epi-sec-linkage-results-neutral`
Status: Active

## Activation

- [x] `issue-284`/`spec-036` completed through final `PR-289` at `commit-40b284f` and canonical closeout `PR-290` at `commit-6ba8f25` before implementation began.
- [x] The predecessor diff, tests and accepted `spec-034` slice-3 contract were reconciled at exact canonical `commit-6ba8f25213569aaffb55f8c10ef544a4800f6de6`.
- [x] The owner-authorised stack-after-green promotion is recorded on `issue-285` and in this specification before package edits.
- [x] The change remains bounded to linkage, projection, result, issue, diagnostic and manifest semantics; registry, universe and final documentation reconciliation remain excluded.

## Linkage And Projection

- [x] Both linkage exports, public arguments, four component names and S3 classes remain.
- [x] New linkage objects have exact neutral schemas, use only explicit `output_action`, make no scaffold retention choice and contain no confirmation/privacy field.
- [x] Exactly the declared identifier is pseudonymised, retained/dropped columns and record keys are structurally consistent, and table/crosswalk invariants remain.
- [x] The exact current-master adapter is syntax-only, warns once, cannot use status/privacy values as authority and rejects actions without executable mappings.

## Results, Issues And Diagnostics

- [x] Every completed audit returns `audit_complete`; expected apply no-commit outcomes return `not_written`; only a committed apply returns `complete`.
- [x] Issues use only `error`/`warning`, omit inferred sensitivity and preserve every substantive technical code/count.
- [x] Governance and blocker conditions/statuses are removed without weakening structural validation or protected rollback.
- [x] Caller-requested `issue_values` are ordinary data, default omission remains value-free and the deprecated alias cannot conflict with an explicit new choice.
- [x] Manifests use `output_type = "pseudonymised_table"` without a sensitivity field, and output dictionary/catalogue handoff remains valid.

## Preserved Integrity

- [x] Exact linkage, crosswalk, record-key, duplicate, token, destination and identity-family behaviour remains covered.
- [x] Transaction ownership, advisory locking, lock transfer/cleanup, rollback, source non-mutation and row reconciliation remain covered.
- [x] PostgreSQL authentication and permission failures remain fixed sanitised technical outcomes.
- [x] No new dependency, real identifier, production database, role/grant administration or unrelated behaviour enters the diff.

## Documentation And Verification

- [x] Roxygen-generated help, installed walkthrough and only directly affected guide snippets match observed behaviour.
- [x] Focused offline and live PostgreSQL tests, package lint, complete local/CRAN checks, online workflow-state check and `git diff --check` pass or exact environment limits are recorded.
- [x] The software-verification, truth-and-semantics and copy-edit checklists are applied as self-review; render/release is applied if the vignette changes.
- [x] Review evidence, compatibility impact and remaining lifecycle state are recorded without claiming merge or closeout.
