# Acceptance

Spec ID: `039-reviewable-qc-cleaning-proposals`

Status: Active

## Planning And Activation

- [x] `issue-271` and roadmap `issue-249` establish this bounded proposal-only outcome and identify `issue-272` as the staged successor.
- [x] The current semantic scaffold/dictionary, preparation, canonical summary and PostgreSQL aggregate contracts were inspected before design.
- [x] Stable opaque variable keys, exact evidence/proposal schemas, state separation, proposal rules, edge cases and privacy boundaries are resolved in the SDD before package implementation.
- [x] The focused pre-code offline baseline passes; unavailable live PostgreSQL baseline execution is recorded without claiming verification.
- [x] PR-298 is merged to canonical `master` as commit-9f48a06 with green checks, accepting the planning contribution before any package source, executable test or generated help change begins.

## Public And Semantic Contracts

- [x] One exported read-only function accepts only a source, reviewed specification and exact caller-managed variable-key map.
- [x] The result contains exact separate `evidence` and `proposals` tables and no cleaned data, approved rule, source-name map or output path.
- [x] Descriptive evidence, pending proposals and future approved rules have distinct state fields, schemas and authorities.
- [x] Semantic type, units, levels, min, max and missing codes remain unchanged and authoritative; empirical bounds use only candidate screening fields.
- [x] Existing scaffold, extended dictionary, preparation and summary public schemas/formals remain compatible.

## Evidence And Proposal Correctness

- [x] Hand-authored fixtures establish counts, missingness, numeric extrema, type-7 fences and tail counts independently of production code.
- [x] Exact observed 0/1 numeric or integer inputs produce only a pending binary candidate and never automatic reclassification.
- [x] Tukey signals produce pending screening prompts only when finite tail counts are positive and never claim scientific validity.
- [x] Units are prompted but not inferred; missing codes and general allowed levels are never inferred or enumerated.
- [x] Binary precedence prevents an imbalanced 0/1 variable from receiving a misleading fence proposal.
- [x] Equivalent in-memory and PostgreSQL fixtures have identical discrete proposals and numerically equivalent evidence under the declared tolerance.

## Edge, Privacy And Non-mutation Contracts

- [x] Zero-column, zero-row, all-missing, missing-variable, mixed/incompatible, unsupported, non-finite, declared-identifier and high-cardinality cases return the exact safe outcomes.
- [x] PostgreSQL high cardinality remains one scalar aggregate row and no categorical/text values, frequencies or observations are collected.
- [x] Outputs, print/structure methods and conditions contain opaque keys and aggregate fields only, with no names, relation identity, row examples, identifier values, category/text values, SQL, credentials or project-specific committed material.
- [x] Inputs and sources remain unchanged; no file, cleaned object, table, rule, privilege or partial result is created on success or failure.
- [x] PostgreSQL execution remains in one read-only repeatable-read snapshot and restores the caller-owned connection to idle state.

## Documentation And Verification

- [x] Roxygen, README, NEWS and the specification-first EDA vignette document the exact result, opaque-key lifecycle, review-only meaning, aggregate disclosure caveat and issue-272 boundary.
- [x] Changed R files are styled and package lint is clean.
- [x] Focused offline and live PostgreSQL tests pass with expected values defined independently.
- [x] `scripts/check-local.sh` passes.
- [x] `scripts/check-cran.sh` passes with only reconciled inherited or external notes, if any.
- [x] `scripts/check-workflow-state.sh` and `git diff --check` pass before handoff.
- [ ] Required pull-request checks are green and actionable feedback is resolved.

## Closeout

- [ ] The implementation pull request closes issue-271 only after every acceptance criterion is delivered.
- [ ] Canonical merge and issue closure are verified before setting this manifest to `completed` and moving spec-039 under `future/specs/done/`.
- [ ] Roadmap, TODO, changelog, acceptance and review evidence are reconciled.
- [ ] `issue-272` is promoted only after `issue-271` canonical closeout; `issue-273` remains staged behind `issue-272`.
