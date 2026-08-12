# Brief

Spec ID: `037-epi-sec-linkage-results-neutral`
Status: Active

## Problem

The retained linkage and pseudonymisation interfaces still require confirmation and privacy classifications, report expected technical no-write outcomes as `blocked` with `blocking` issues, convert runtime metadata drift into governance conditions, infer sensitivity in manifests and diagnostic rows, and automatically hide caller-requested identifier values. Those behaviours exceed the package authority boundary accepted in `spec-034` and retained as the explicit successor boundary after `issue-284`/`spec-036`.

## Objective

Implement `issue-285` as the third behavioural slice from completed `spec-034`: callers select output actions and whether identifier-value diagnostics are returned, while episcout validates structural and database-integrity requirements, preserves exact linkage and atomic write behaviour, and returns neutral technical results without confirmation, privacy, governance, blocker or automatic-hiding semantics.

## Observable Outcome

`epi_sec_linkage_scaffold()` and `epi_sec_linkage_spec()` use only explicit `output_action` metadata. `epi_sec_pseudonymise_db()` returns `audit_complete` for every completed audit, `not_written` when technical error-severity issues prevent a requested apply, and `complete` only after commit. Optional `issue_values` are an ordinary data frame, the compatibility `sensitive_issues` argument is deprecated and cannot override an explicit conflicting choice, issues omit inferred sensitivity, and manifests use `output_type = "pseudonymised_table"`.

## Authority And Baseline

The repository owner promoted `issue-285` and authorised stack-after-green execution from exact canonical `commit-6ba8f25213569aaffb55f8c10ef544a4800f6de6` after `issue-284` completed through final `PR-289` at `commit-40b284f` and closeout `PR-290` at `commit-6ba8f25`. Read-only reconciliation confirms the predecessor removed registry, crosswalk and output privilege policy while intentionally leaving linkage, result, issue, diagnostic and manifest semantics for this slice. The online workflow-state check passed before activation, and the focused offline baseline passed with only the three explicit live PostgreSQL gates skipped.

## Scope

- Linkage schemas, constructors, validators and printers in `R/epi_sec_linkage.R`.
- Pseudonymisation result, issue, condition, diagnostic, manifest and output-action consumers in `R/epi_sec_pseudonymise_db.R`.
- The stable pseudonym helper cross-reference, focused offline/live tests, generated help, installed walkthrough and only directly required guide snippets.
- `spec-037` and normal active-lifecycle records.

## Exclusions

- Registry privilege/creation behaviour completed by `issue-284`/`spec-036`.
- Identity-universe behaviour completed by `issue-278`/`spec-035`.
- Broad or final longitudinal documentation reconciliation reserved for `issue-269`.
- Core EDA behaviour, role/grant administration, new dependencies, real identifiers, production databases, fuzzy linkage or new diagnostic value families.

## Recovery

The change introduces no database migration and changes no registry or output row directly. Apply remains transactionally atomic, no-write outcomes preserve existing registry/output state, and the scoped commit can be reverted without deleting database objects or data.
