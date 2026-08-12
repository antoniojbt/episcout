# Review

Spec ID: `041-reviewed-civil-date-derivation`

Status: Completed

## Review Scope

Review the explicit semantic-declaration boundary, strict local timestamp grammar, exact-midnight validation, missingness preservation, source immutability, atomic file publication, transactional PostgreSQL publication, timezone absence, aggregate reconciliation and privacy-safe failures.

## Authority And Evidence

Issue-273 is the accepted product and privacy contract. Completed issue-272/spec-040 establishes the separately approved operation and atomic publication pattern. Literal hand-authored local timestamps and Gregorian calendar expectations define important expected results independently of production helpers. Existing R and PostgreSQL code is implementation evidence only.

## Checklist Status

- Software verification: pass. Exact interfaces, invalid inputs, immutable operation objects and sources, no-replace files, aggregate-only midnight blocking, transaction rollback and final dimension/type/missingness reconciliation are covered by focused tests; package lint and the complete local check pass.
- Truth and semantics: pass against issue-273 and the completed spec-040 approval boundary. Civil-date meaning is declared as `civil_date`; storage and midnight observations never infer it. Strict character timestamps and PostgreSQL `timestamp without time zone` remain calendar-local, while `POSIXct`, `POSIXlt`, offsets, zones and `timestamp with time zone` are rejected.
- Analysis and statistics: pass. Literal leap-day/year-boundary fixtures define expected dates independently; source-to-derived missingness, row counts and added-column counts reconcile in memory, after file publication and against a disposable PostgreSQL 18.4 destination. No inferential method, truncation or correction is introduced.
- Copy-edit: pass. Roxygen help, README, NEWS, project map, workflow records and the specification-first guide use the exact interface and British English and preserve the temporal/privacy limits.
- Render and release: pass for local review. The changed specification-first vignette rendered with the development package; the civil-date heading, schema example, preserved timestamp, derived dates and missing value were inspected. No publication was performed.

## Verification Evidence

- Hand-authored in-memory fixture: `2024-02-29 00:00:00`, `2024-12-31 00:00:00.000`, missing and `2025-01-01 00:00:00` derive literally to the corresponding dates and missing value. Separate `00:00:01`, `12:00:00` and `00:00:00.001` values produce a literal aggregate failure count of three without value/name disclosure.
- Focused non-database tests pass with only the expected PostgreSQL-gate skip. Focused tests and the combined predecessor-cleaning regression pass without skips against a disposable PostgreSQL 18.4 cluster, including aggregate blocking for fractional-second and non-finite local timestamps.
- Targeted live-backend coverage is 98.07%. The only instrumentation gaps are fixed-schema and reconciliation stop blocks; direct mocked failure assertions exercise those conditions even though `covr::file_coverage()` resolves the already loaded namespace closures rather than its instrumented copies.
- `styler::style_file()` reports the changed R/test files styled and package-loaded `lintr::lint_package()` reports no findings.
- `scripts/check-local.sh` passes documentation, lint, the complete ordinary test suite, source build, examples, vignette rebuild and R CMD check with zero errors and warnings. R CMD check reports only `unable to verify current time`, an environment feasibility NOTE unrelated to package behaviour.
- `scripts/check-workflow-state.sh` matched live GitHub state before publication; it is rerun at PR-303 handoff with spec-041 as the sole implementation in review and issue-273 open.

## Privacy Review

The approved operation object necessarily contains caller-owned derived names and approval references and must remain caller-controlled. Application results return only the explicitly requested complete data plus an aggregate audit keyed by opaque source identifiers. Fixed validation and database errors do not echo source or derived names, paths, relations, timestamps, row identifiers or approval references. Non-midnight failure contains one aggregate count only. Canary searches found no test canary outside executable tests, and the production SQL contains only quoted identifiers, scalar counts, local `::time` validation and local `::date` derivation.

## Compatibility And Limits

Existing approved-cleaning, dictionary, preparation and PostgreSQL compatibility interfaces are unchanged. In-memory local timestamps are limited to exact base character storage with a strict four-digit Gregorian grammar; `POSIXct`/`POSIXlt` remain unsupported because they encode instant/timezone semantics. CSV cannot preserve the R `Date` class; the returned data and RDS do. PostgreSQL `CREATE TABLE AS` does not copy source constraints, indexes or privileges and has no physical row-order contract. RData, Parquet, overwriting, correction, rounding, truncation and timezone conversion remain outside spec-041.

## Open Questions

None. In-memory `POSIXct`/`POSIXlt` rejection is required by the no-instant/no-timezone boundary; strict character local timestamp storage is the smallest base-R representation that preserves the authorised semantics.

## Handoff State

PR-303 passed PostgreSQL integration, macOS and Ubuntu package checks, coverage, CodeQL, Actions analysis, CodeFactor and both informational Codecov statuses at final commit-a1f1dff, then merged to canonical `master` as commit-db839d0 and closed issue-273. No unresolved review thread or actionable finding remained. Spec-041 is completed and archived; issue-273 is terminal and has no automatic successor. The implementation review above was a self-review; the final PR diff received a separate lifecycle review before merge.
