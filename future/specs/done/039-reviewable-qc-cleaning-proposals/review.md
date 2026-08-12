# Review Notes

Spec ID: `039-reviewable-qc-cleaning-proposals`

Status: Completed

## Planning Findings

- The lean data-frame scaffold contains only the 15 semantic fields and deliberately returns no observed evidence. The extended PostgreSQL dictionary has stable source keys, but those keys contain source names and therefore cannot be reused in the aggregate proposal result.
- Canonical data-frame and PostgreSQL summaries already establish the required numeric definitions, but direct reuse of the complete canonical result would return source names and collect all categorical frequencies. Spec-039 therefore requires a dedicated scalar aggregate QC path with the same numeric semantics.
- Semantic `min` and `max` are currently descriptive metadata: validation checks their ordering and preparation does not apply them. Candidate screening bounds need different names so an empirical fence cannot become a cleaning rule through field reuse.
- Caller-managed opaque keys are the only design that remains stable across backends and semantic edits without adding fields to the reviewed dictionary or producing guessable name-derived hashes. The lookup-name crosswalk stays input-only.
- Exact 0/1 candidacy can be established from aggregate type, distinct count, infinity count and extrema without collecting a level list. Requiring both levels prevents constant zero or one variables from receiving a vacuous binary proposal.
- Tukey fences on imbalanced binary data can mark a legitimate level as a tail. Binary candidacy therefore takes precedence and suppresses a fence proposal for the same variable.
- Missing sentinel meaning and units cannot be inferred from names, common values or frequency. Generated unit candidates and missing-code candidates remain blank; an absent unit may create a review prompt only for types where units can apply.
- High-cardinality safety requires `COUNT(DISTINCT ...)` only. A grouped frequency query that is discarded later would still violate the collection and scalability boundary.
- The result's aggregate numeric evidence can remain sensitive despite opaque keys. The first implementation has no persistence or runner/report integration and must state that aggregate output is not automatically safe to share.

## Semantic Decisions

- The reviewed specification is authoritative; generated evidence and pending proposals never modify or override it.
- Evidence has state `descriptive`; generated proposals have state `pending`; approved rules are a separate successor schema and authority.
- The public result contains no approved-rule adapter, cleaned data or automatic output.
- Identifier-role variables are explicitly not profiled; the function does not infer identifier status from names, uniqueness or constraints.
- Observed numeric extrema and Tukey fences are finite-only descriptive evidence. Screening proposals require at least one finite tail count and never use validity terminology.
- `0;1` is the sole generated allowed-level string. No other observed value is returned as a level or missing-code candidate.
- Per-variable incompatibility is represented by fixed codes and typed unavailable aggregates. Structural input failures abort without a partial object.

## Checklist Application

| Checklist | Planning application | Current status |
| --- | --- | --- |
| `checklists/truth-and-semantics.md` | Separates reviewed metadata, observed evidence, pending candidates and future approved rules; fixes missingness, extrema, binary and fence meanings. | Passed through implementation and final review. |
| `checklists/analysis-and-statistics.md` | Fixes unit of observation, denominators, finite/missing partitions, type-7/Tukey definitions, high-cardinality handling and independent fixture values. | Passed with literal fixtures and live cross-backend parity. |
| `checklists/copy-edit.md` | Uses British English, consistent state/field terminology, explicit limitations and no hard-wrapped prose paragraphs or project-specific examples. | Passed for final user documentation. |
| `checklists/software-verification.md` | Routes executable verification for the public package interface. | Passed through focused, live, local, CRAN-oriented and hosted checks. |

This checklist review is self-review and is not independent of the specification authoring.

## Baseline Evidence

- Canonical `master`, `origin/master` and `upstream/master` are exact at `a1eabaf77907f31ed1f6af9c1a51a8867b82950a`.
- `scripts/check-workflow-state.sh` passes online after configuring the canonical `upstream` remote and confirms issue-249 is open, completed manifests match GitHub and no implementation specification is active or in review.
- `scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(spec-scaffold|summaries|postgres-parity)|db-dictionary', reporter = 'summary')"` passes the offline dictionary, scaffold and summary coverage; nine live PostgreSQL tests skip behind the documented environment gate.
- PostgreSQL client 18.4 is available, but `127.0.0.1:5432` has no responding server and the current process cannot access the Docker socket. No live baseline is claimed.
- Source, tests, completed specs 003/008/012/013/030 and the staged issue-272/issue-273 contracts were inspected as compatibility and semantic evidence. Existing code and tests were treated as prior behaviour, not independent scientific authority.

## Planning Verification

- `scripts/check-local.sh` passes after the draft is written: package lint has no findings, the complete offline suite passes with 26 documented environment-gated skips, and R CMD check completes with 0 errors, 0 warnings and one environment NOTE because current time could not be verified.
- Check-generated roxygen and disabled-snapshot side effects were inspected and restored; the staged change contains planning records only.
- `scripts/check-workflow-state.sh --offline`, `scripts/check-workflow-state.sh` and `git diff --check` pass with `spec-039` as the sole draft implementation specification.
- Live PostgreSQL and `scripts/check-cran.sh` are not run for this planning-only contribution. They remain mandatory implementation checks in the manifest and acceptance record.

## Open Questions

None blocking the bounded implementation after the planning review gate. `issue-272` retains authority over the exact executable approved-rule schema, provenance and application workflow; `spec-039` fixes only the separation boundary needed to prevent pending proposals from being treated as approved.

## Planning Scope Confirmation

This contribution changes only spec-039 and synchronised planning records. It does not change package source, tests, documentation generated from roxygen, dependencies, runtime behaviour, data, PostgreSQL state, GitHub issues or pull requests.

## Implementation And Closeout Notes

PR-298 merged the accepted planning contract to canonical `master` as commit-9f48a06 with all required checks green. Spec-039 is active for the bounded implementation on issue-271; issue-272 remains the staged successor.

The implementation adds `epi_eda_qc_proposals()` with exact `data`, `spec` and `variable_keys` formals, an exact two-table result, aggregate-only printing/structure display and no persistence or rule-application path. The in-memory path uses the canonical missing mask and type-7 numeric core after fixed storage compatibility checks. The PostgreSQL path uses only one-row count/numeric aggregates plus a one-row fence-count follow-up inside the existing read-only repeatable-read transaction and never invokes categorical frequencies, Shapiro collection, plotting or catalogue-value profiling.

Hand-authored neutral fixtures independently fix the `c(1, 2, 3, 100)` quartiles at 1.75 and 27.25, fences at -36.5 and 65.5, and one upper-tail observation. Tests cover exact 0/1 binary precedence, all-zero/all-one/three-level/signed/non-finite exclusions, zero-IQR tails, declared missing codes, zero rows, numeric and text all-missing inputs, absent variables, incompatible storage, list/matrix/raw/complex/semantic subclasses, declared identifiers, date/datetime unit prompts, high cardinality, data.table by-reference safety, exact schemas and value-free errors. Expected values are literal and were not generated through the production path.

The focused offline selection `scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(qc-proposals|spec-scaffold|summaries|postgres-source)|db-dictionary', reporter = 'summary')"` passes with only the expected live-PostgreSQL skip. Package-loaded `lintr::lint_package()` returns no findings. Direct object inspection confirms specification-order opaque keys, fixed states/codes, typed unavailable fields, blank inferred unit/missing-code/general-level fields and no source names or row canaries in the result, print or structure output.

An isolated trust-local PostgreSQL 18.4 cluster under a temporary directory ran `EPISCOUT_TEST_POSTGRES=1 ... devtools::test(filter = 'eda-(qc-proposals|postgres-parity)', reporter = 'summary')`; the complete new and existing parity selection passes. Equivalent fixtures have identical discrete evidence and proposals, numerical evidence within the declared tolerance, an unchanged relation, a valid idle caller connection and no row-valued QC query. The temporary clusters were stopped and deleted after verification.

The specification-first vignette renders successfully and the reviewed HTML contains the opaque-key example, review-only state distinction and aggregate disclosure warning. `scripts/check-local.sh` passes the complete suite, lint, build, vignette rebuild and R CMD check with 0 errors, 0 warnings and one environment NOTE because current time could not be verified. `scripts/check-cran.sh` passes build, tests, examples, vignette rebuild and HTML/PDF manual checks with 0 errors, 0 warnings and one inherited incoming-feasibility NOTE for new-submission status, the existing absent prebuilt vignette index and two existing Stack Overflow URLs returning 403. Known roxygen author-format churn and disabled-vdiffr snapshot cleanup were restored exactly and are not part of the change.

Privacy-canary searches of package source, help, README, NEWS, vignette, project map and specification records find no committed row/source canaries. The result necessarily contains caller-supplied opaque keys and aggregate numeric evidence, which can remain sensitive and is explicitly documented as requiring disclosure review before saving or sharing. No dependency, semantic dictionary field, existing public formal, cleaned data, database object, output bundle, approval adapter or issue-272 rule schema is added. This implementation review is self-review and is not independent.

PR-299 passed PostgreSQL integration, macOS and Ubuntu package checks, coverage, CodeQL, Actions analysis and CodeFactor at final commit-8d40d45, then merged to canonical `master` as commit-78a4776 and closed issue-271. No unresolved review thread or actionable finding remained. Spec-039 is completed and archived; issue-272 is promoted only after this closeout becomes canonical, while issue-273 remains staged behind it.
