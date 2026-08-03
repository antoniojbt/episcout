# Review Notes

Spec ID: `013-specification-guided-data-preparation`
Status: Completed

## Planning Findings

- Current schema, missingness and canonical summary paths assess reviewed declarations without producing a prepared dataset.
- Canonical sentinel matching already compares non-missing character representations, so preparation must reuse it and separately count standard versus sentinel missingness.
- Existing schema coercibility is intentionally descriptive and is broader than this preparation contract; character numeric parsing remains blocked without locale rules.
- Local ISO datetime strings currently have no specification timezone contract, so spec 013 introduces an optional row-level `timezone` field while keeping the four required specification fields unchanged.
- Preparation audit needs dataset rows without colliding with source variables, so `.dataset.` is a reserved name prefix.
- Returning blockers rather than throwing for observed incompatibility allows audit and blocked apply to report all findings while keeping the top-level return contract stable.
- An untouched scaffold carries `review_required` evidence; apply must gate on explicit `reviewed` values when that evidence column is present, while audit and older specifications remain compatible.
- The existing semicolon-delimited missing-code format cannot encode blank, whitespace-only or semicolon-containing sentinels; v1 documents that limit rather than inventing an incompatible escape rule.

## Approved Semantic Decisions

- Audit returns original data and no after-schema; blocked apply does the same.
- Apply is all-or-nothing and plans every action before creating its internal copy.
- Missing or `NA` requiredness is optional/unasserted; only explicit `TRUE` makes absence blocking.
- Character-to-numeric/integer parsing is blocked, including all-sentinel character vectors.
- Unexpected append order is deterministic, observations are retained and audit warns that data diverge from the unchanged reviewed specification.
- Append applies only to categorical variables; binary unexpected values always block to preserve the two-level contract.
- Offset/Z character datetimes normalise UTC; local datetimes require a valid reviewed timezone and ambiguous/nonexistent local wall times block.
- Audit and errors never include raw values. Dataset rows use reserved `.dataset.` names.
- The in-memory core performs no file output and does not attempt PII detection, identifier rewriting or anonymisation.
- Optional `min`/`max` fields are not preparation rules in v1 and never cause clamping, recoding or conversion failure.

## Approval And Checklist Routing

On 2026-08-03 the repository owner instructed implementation of issue #182 as part of the ordered EDA work. This instruction approves and activates the completed spec 013 planning contract, subject to stopping for a later ambiguity that materially changes privacy, scientific meaning or the public interface.

| Checklist | Application | Required evidence |
| --- | --- | --- |
| `software-verification.md` | Public formals, stable return/audit schemas, all-or-nothing planning, non-mutation, errors and integration | Focused/full tests, realistic invocation, inspected objects, compatibility impact and recovery behaviour |
| `truth-and-semantics.md` | Sentinel meaning, requiredness, conversion validity, categories, datetime timezone and preparation claims | Explicit policies, independent expectations, provenance, unresolved questions and limitations |
| `analysis-and-statistics.md` | Counts, duplicates, missingness reconciliation, categorical levels and transformed analytical inputs | Hand-derived counts, source/output reconciliation, denominators, excluded/blocked rows and invariants |
| `copy-edit.md` | Roxygen, README, NEWS and vignette | Exact public terms, privacy limitations, British English, example consistency and contextual review |

Checklist evidence here is implementation self-review unless an independent reviewer repeats it.

## Baseline Evidence

- Inherited baseline: spec 012's final `scripts/check-local.sh` passed on the same starting commit with zero errors, zero warnings and zero notes; the two known environment skips remained and generated side effects were restored.
- Current baseline: `scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda_schema|eda_missing|eda-spec|eda-summaries', reporter = 'summary')"` passed before production edits on this branch.
- Interpretation: executable baselines establish prior state only and do not validate preparation semantics independently.

## Open Questions

None blocking implementation under the approved contract. Any platform limitation in reliably detecting ambiguous daylight-saving local wall times must be resolved or narrowed explicitly before accepting local datetime conversion.

## Implementation Review

Implemented `epi_eda_prepare()` as a five-component `epi_eda_preparation` result with a complete planning pass before any apply conversion. Audit and blocked apply return the exact input object and no after-schema. Successful apply returns an ordinary data frame, preserves rows and row names, orders specification variables first, and follows explicit extra-variable policy.

The implementation reuses canonical sentinel comparison, adds stricter private preparation validators, and makes one narrow schema alignment: an ordinary factor whose two levels exactly match a binary declaration is now `compatible`, allowing successful prepared output to satisfy the promised after-schema invariant. Character numeric parsing, invalid integer conversion, unsafe factor metadata, unexpected binary observations, unsupported nested columns, absent required variables, unreviewed scaffold evidence, invalid timezones and ambiguous/nonexistent local wall times remain value-free blockers.

Two independent read-only reviews found and drove fixes for historical timezone rollbacks, fractional local timestamps, arbitrary minute offsets, leap-second normalisation, masking before integer coercion, trailing empty levels, absent-variable totals, dropped-extra change counts, stage ordering and comprehensive unsupported-column audits. Final re-review reported no remaining blocker.

## Verification Evidence

- Before implementation, the focused schema/missingness/specification/summary baseline passed.
- The test-first preparation suite initially failed because the API did not exist, then passed with 101 expectations after implementation and review fixes. It covers stable formals/results, audit/apply non-mutation, all-or-nothing blockers, all supported types, typed sentinels, integer boundaries, categorical append, binary safeguards, required/optional variables, extra policies, duplicates, review gating, strict temporal conversion, historical and modern timezone ambiguity, arbitrary offsets, zero rows, nested columns, privacy, data.table non-mutation and canonical EDA reconciliation.
- The focused preparation/schema/specification/summary/workflow regression command passed.
- Package-loaded `lintr::lint_package()` reported no findings.
- Direct probes independently confirmed exact `+01:01` and `-05:30` UTC arithmetic, fractional local time preservation, modern nonexistent/fold detection, 23-hour Kwajalein and 2,079-second Dublin historical fold detection, and value-free blocked results.
- Final `scripts/check-local.sh` passed: full suite with the two known environment skips, vignette build, examples and package check completed with 0 errors, 0 warnings and 0 notes.
- Final `scripts/check-cran.sh` completed with 0 errors and 0 warnings. Its single incoming NOTE is external/inherited: new submission and no prebuilt vignette index, plus two Stack Overflow documentation URLs returning HTTP 403.
- Check-generated unrelated Rd and disabled snapshot drift were restored; `git diff --check` passed.

## Closeout Notes

Spec 013 is complete. The implementation adds no dependency or file-writing path, changes no pseudonymisation behaviour, and creates no tag or release. Optional `min`/`max` remain descriptive, and blank/whitespace/semicolon missing sentinels remain a documented v1 encoding limitation.
