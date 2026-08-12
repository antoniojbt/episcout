# Review

Spec ID: `040-approved-cleaning-rules-and-processed-outputs`

Status: Review in PR-301

## Review Scope

Review the exact approved-rule boundary, deterministic rule hash, typed missingness transitions, immutable source behaviour, atomic file publication, transactional PostgreSQL publication, aggregate reconciliation, privacy-safe conditions and equivalence of supported backends.

## Authority And Evidence

Issue-272 is the accepted product contract. Completed issue-271/spec-039 establishes that evidence is descriptive, proposals are pending and approved rules require a separate successor-owned schema. Existing EDA missingness and PostgreSQL compatibility helpers are implementation evidence only; literal hand-counted fixtures define expected transitions independently.

## Checklist Status

- Software verification: pass. Exact interfaces, invalid inputs, no-replace file publication, immutable sources, transaction rollback and final reconciliation are covered by focused tests; package lint and the complete local check pass.
- Truth and semantics: pass against issue-272 and the accepted spec-039 boundary. Only caller-authored `rule_state = "approved"` objects execute; descriptive dictionary fields and pending proposals are rejected. Standard missing values remain missing and only non-missing invalid observations contribute to transitions.
- Analysis and statistics: pass. Literal fixtures independently define the important masks and counts; dimensions and missingness reconcile in memory, after CSV/RDS publication and against a disposable PostgreSQL 18.4 destination. No inferential or scientific imputation method is introduced.
- Copy-edit: pass. Roxygen help, README, NEWS, project map, workflow records and the specification-first guide use the exact interface names, British English and the implemented scope.
- Render and release: pass for local review. The updated specification-first vignette renders and is rebuilt successfully by R CMD check; its new approved-rule section, example audit and subsequent headings were inspected. No publication was performed.

## Verification Evidence

- Hand-authored fixture: numeric values `-1, 0, 5, 11, 999, NA` under inclusive bounds 0–10 and missing code 999 produce three transitions, one missing before and four missing after. The categorical fixture `A, B, X, M, NA, A` under allowed values A/B and missing code M produces two transitions, one missing before and three missing after. Tests assert these literal values independently of production helpers.
- Focused non-database tests pass with the PostgreSQL test correctly skipped when its gate is absent.
- Focused tests pass without skips against a disposable local PostgreSQL 18.4 cluster, including equivalent processed values and audit rows, collision refusal, zero-row publication and forced post-creation reconciliation rollback.
- `styler::style_file()` and package-loaded `lintr::lint_package()` report no findings.
- `scripts/check-local.sh` passes documentation, lint, the complete ordinary test suite, package build, vignette rebuild and R CMD check with zero errors and warnings. R CMD check reports only `unable to verify current time`, an environment feasibility NOTE unrelated to package behaviour.
- `scripts/check-workflow-state.sh` matched live GitHub state before publication; it is rerun at PR-301 handoff with spec-040 as the sole implementation in review and issue-273 retained as successor.

## Privacy Review

The approved-rule object necessarily contains opaque keys, executable values and an opaque approval reference and must remain caller-controlled. Application results and display methods return only the processed data explicitly required in memory plus a rule hash, opaque-keyed counts, dimensions, reconciliation and publication kind. Fixed validation and database errors do not echo names, paths, relations, allowed/disallowed values, keys or approval references. Canary searches of production source, generated help, README, NEWS, vignette, project map and spec records found no row/source canaries; neutral synthetic canaries remain confined to executable tests.

## Compatibility And Limits

Existing public formals and semantic dictionary fields are unchanged. CSV is an explicit interchange export and cannot retain R vector classes; the returned object and RDS output retain supported source storage. PostgreSQL creates columns through `CREATE TABLE AS` and does not copy constraints, indexes or privileges. PostgreSQL relations have no physical row-order contract. Text/date/datetime rules, empty-string or semicolon-bearing codes, RData, Parquet, overwrite, filtering, imputation, winsorisation and civil-date derivation remain outside spec-040.

## Handoff State

PR-301 is the draft implementation pull request for issue-272 and spec-040. Merge, issue closure, canonical closeout and promotion of issue-273 remain pending. The implementation review above is a self-review and is not independent; the final PR diff is reviewed separately before merge.
