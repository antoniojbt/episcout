# Agent Guidance Impact Review and Portable Execution Plan

- Date: 2026-07-31
- Status: Planning and handoff record
- Audience: A future Codex session or human contributor working from another clone of `episcout`
- Repository-relative base reviewed: `master` at `b03ed07`

## Purpose

This document consolidates the package impact assessment created after adopting the repository `AGENTS.MD` and routed checklists. It records which earlier truth-review findings have already been repaired, which current decisions supersede older recommendations, which package areas still need independent validation, and the ordered instructions for continuing the work from another machine.

This document is portable. Every path and command is relative to the repository root. Do not copy machine-specific home directories, environment locations or temporary paths into code, documentation, fixtures or plans.

Do not implement every finding in this document in one branch. The repository requires one numbered SDD/TDD specification at a time. The current first implementation candidate is spec `010-canonical-eda-summary-contract`; all unrelated findings remain separate candidate work until promoted through `future/TODOs.md`.

## Required Reading and Authority

Read these sources in order before changing package code:

1. `AGENTS.MD` for repository-wide contribution, correctness, testing, documentation and Git rules.
2. `checklists/README.md` and the routed checklist files applicable to the task.
3. `future/README.md`, `future/TODOs.md` and `future/decisions.md` for the active planning workflow and existing decisions.
4. Every document under `future/specs/done/010-canonical-eda-summary-contract/` when working on spec 010.
5. `future/reviews/done/robots_output_truth_review/20260729_final/truth_review_report.md`, `truth_review_catalog.csv`, `truth_review_repair_plan.md` and `categorical_presentations_review.md` as review evidence, subject to the staleness warnings below.
6. Current source, tests, generated documentation and Git history as evidence of the implementation and released interface.

Direct user instructions and newly approved decisions take precedence over this handoff. Existing code, tests, fixtures, snapshots and previous outputs establish prior behaviour but are not independent proof of analytical correctness.

## Current Repository State and Important Corrections

At the reviewed base, spec 010 exists with status `Draft`, no spec is marked active, and `future/TODOs.md` identifies updating and implementing spec 010 as Priority 1 work. The spec manifest prescribes base branch `master` and working branch `refactor/canonical-eda-summary-contract`.

The earlier package-impact discussion recommended retaining compact v1 and typed v2 summary contracts. That recommendation is superseded by spec 010. Spec 010 records the later decision to replace the unreleased v1/v2 interface with one canonical typed contract, remove `summary_version`, retain no legacy adapter and accept the return-shape break because external compatibility is not required. Do not reintroduce compact/typed aliases, compatibility warnings or dual output paths while implementing spec 010.

The truth-review report and catalogue were written before repair commit `f6d68d5`. Do not treat all original `gap`, `partial` or `contradicted` statuses as current. Reconcile them against current code and tests before planning repairs.

The following confirmed repairs are already present and must not be reimplemented:

- `TR-011`, scoped repair: `epi_stats_numeric(na.rm = FALSE)` now validates and honours the argument by retaining factual count fields and returning typed unavailable analytical fields when missing values are not removed.
- `TR-016`: EDA plots apply specification-aware missing masks so declared sentinel codes do not remain in analytical plots.
- `TR-021`: `epi_stats_prop_outcome()` derives numerator and denominator from the same analysis-window subset and validates eligible outcomes.
- `TR-030`: `epi_clean_transpose()` uses `colnames(df)[-id_col_num]` so arbitrary identifier-column positions retain correct row labels.
- `TR-031`: `epi_clean_merge_nested_dfs()` defaults to a full outer join through `all.x = TRUE, all.y = TRUE` and retains explicit legacy left-join behaviour.
- `TR-032`: `epi_clean_spread_repeated()` supports zero and nonconsecutive visit codes, retains visit codes as names, rejects missing visit codes and rejects duplicate identifier/visit pairs before reshaping.
- `TR-002` to `TR-005`, implementation repair: EDA schema output now separates historical presence `status` from `type_status` and `type_reason` and reports compatibility without coercing data.

The truth-review catalogue should eventually be updated to record these repairs, their discriminating tests and the remaining limitations of their evidence. Catalogue reconciliation is documentation work after the corresponding accepted implementation is stable; it is not a reason to change repaired production code again.

## Findings That Directly Affect Spec 010

Spec 010 is aligned with the new instructions because it replaces two overlapping summary paths with one canonical source of truth, explicitly defines missingness and denominators, requires independent expected values and routes in-memory, CSV and rendered report outputs through the same verified object. The spec is already detailed, but it must be reviewed against all six checklists before human approval.

### Required checklist mapping

Record the following evidence in `future/specs/done/010-canonical-eda-summary-contract/review.md` during implementation:

| Checklist | Application to spec 010 | Evidence required |
| --- | --- | --- |
| `truth-and-semantics.md` | Missingness, finite-value exclusions, categorical declarations, denominators, temporal parsing, absent variables and all-missing totals | Authoritative definitions, explicit semantic decisions, hand-derived fixtures and unresolved items |
| `analysis-and-statistics.md` | Numeric, categorical, text and temporal summary values | Independent calculations, source-to-output reconciliations, row counts, denominators, warnings and limitations |
| `software-verification.md` | Public formals, canonical builder, failure behaviour, CSV writes and package integration | Focused tests, realistic invocations, full checks, inspected outputs and compatibility consequences |
| `figures.md` | EDA plots presented by the report and plot dispatch after missing-variable filtering | Reconciled plot-layer data, labels, scales, sentinel handling and rendered inspection where figures appear |
| `copy-edit.md` | Roxygen, README, NEWS, vignette, report prose and captions | Files reviewed, terminology changes, source checks, links and unresolved editorial issues |
| `render-and-release.md` | HTML report, generated Rd, source tarball and machine-readable CSV artifacts | Exact artifacts inspected, render method, pages or sections checked, source-package contents and release decision |

The spec review must state that these are self-checks unless a different reviewer independently repeats them.

### Human approval gate

Spec 010 currently authorises planning only. Before changing package code, obtain explicit human approval of `brief.md`, `sdd.md`, `tdd.md` and `acceptance.md`, and mark the spec status consistently in its documents and manifest. This request for a portable handoff does not itself approve implementation.

If approval changes any semantic policy, public interface or scope boundary, update all spec documents before writing tests. Do not allow production changes and spec decisions to drift apart.

### Authorised public interface changes

After approval, spec 010 authorises these breaking changes and no others:

- `epi_eda_profile_summaries(data, spec)` removes `summary_version` and always returns `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped` in that order.
- `epi_eda_run()` removes `summary_version`, stores the canonical object in `summaries` and writes exactly the six canonical CSV components when `output_dir` is supplied.
- `epi_eda_render_report()` removes `summary_version` and renders the canonical returned object without recalculating statistics.
- `epi_stats_summary(output = "typed")` continues to exist and uses the same canonical builder after constructing an inferred specification and applying its global sentinel codes.
- `epi_stats_summary(output = "current")` retains its unrelated public schemas and behaviour except for the separately authorised no-observed-total correction flowing through numeric summaries.
- `epi_stats_numeric()` retains its formals and output schema, but `sum` becomes typed `NA` when there are no finite observed values, including all-missing, sentinel-only, infinite-only and zero-row inputs.
- No v1/v2 adapter, alias, warning-only compatibility layer, alternate compact path or release operation is part of spec 010.

### Canonical semantic policies

Implement and test these policies exactly unless the approved spec is amended first:

- Specification variable names, types, missing sentinels, categorical levels and ordering are authoritative declarations; observed data supply values and counts.
- `NA`, `NaN` and configured sentinels are missing. Empty and whitespace-only text remain observed unless configured as sentinels.
- `n` is source-vector length, `n_missing` is standard plus sentinel missingness, and `n_observed = n - n_missing`.
- An absent variable has unavailable typed counts rather than the dataset row count and has an explicit skipped reason with required/optional context where known.
- Infinities are observed but not finite. They are counted in `n_infinite`, excluded from finite analytical statistics and never cause an all-infinite vector to report `sum = 0`.
- Numeric location, spread, shape, normality, fences and outlier percentages use the spec-defined finite-value policies and denominators.
- Specification levels are the sole source of declared categorical levels. Declared zero-count levels remain, unexpected observed values are appended deterministically and marked, and unused factor metadata does not create output rows.
- `p_total` uses all source rows and `p_observed` uses all non-missing, non-sentinel observations. Zero denominators return typed `NA`.
- Literal text such as `"NA"` remains observed unless explicitly declared missing.
- Invalid non-missing temporal values produce a per-variable skip with a reason; they are not silently converted to missing.
- Date ranges use days. Datetime values are displayed in ISO UTC form with source timezone metadata and ranges use seconds.
- Every specification row appears exactly once in `variables` and is represented either in one successful type component or in `skipped`, never both and never neither.

## Portable Execution Instructions for Spec 010

### 1. Establish repository and environment state

Run from the repository root:

```bash
git branch --show-current
git status --short
git log -1 --oneline --decorate
git remote -v
scripts/rscript_env_caller.R -e "R.home(); .libPaths(); sessionInfo()"
```

Do not assume the reviewed commit remains current. Compare changes since `b03ed07`, re-read `future/TODOs.md`, check whether spec 010 was already approved or implemented, and stop if later work supersedes this plan.

Do not discard or absorb unrelated worktree changes. If the worktree is dirty, identify ownership and either work around the changes or ask before touching overlapping files.

If the wrapper cannot find R on the new machine, create or update the documented environment from `environment.yml`, or set `EPISCOUT_RSCRIPT` to a verified R binary. Do not edit project files merely to encode a machine-specific R path.

### 2. Approve and activate spec 010

Review the complete spec against `AGENTS.MD` and the six checklist files. Add the checklist-to-evidence mapping above to the spec review if it is not already represented. Resolve any human changes, mark the spec active consistently and retain `master` as the base branch.

Create or switch to the prescribed branch only after approval:

```bash
git switch master
git pull --ff-only
git switch -c refactor/canonical-eda-summary-contract
```

Do not run `git pull`, push, publish or open a pull request without the required repository access and user authorisation. If the branch already exists, inspect it rather than creating a duplicate or deleting it.

### 3. Record the baseline before package-code changes

Record command, R version, package version, result, warnings, skips, notes and worktree effects in the spec review. Run focused tests first:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-summaries|run_eda|eda_report|db-dictionary', reporter = 'summary')"
scripts/check-local.sh
```

Run `git status --short` before and after the baseline. `scripts/check-local.sh` calls `devtools::document()` and the full test suite, so it may expose pre-existing generated-document drift. The globally skipped `tests/testthat/test-plotting.R` can also cause testthat to prune tracked vdiffr snapshots. Do not include those deletions unless visual-regression removal is separately approved; restore only snapshot deletions caused by the run and report the behaviour.

Do not call a passing baseline independent validation. It establishes execution state only.

### 4. Write independent expectations before production changes

Create small, readable test inputs with literal expected outputs. Do not call `episcout`, its shared cores, current snapshots or copied production decision trees to generate expected values.

At minimum cover:

- Numeric finite values plus ordinary missingness, a sentinel and infinity, with hand-reconciled counts, sum, quantiles, mean, sample variance and standard deviation, fences, outlier numerator and outlier denominator.
- Zero-row, all-missing, sentinel-only, infinite-only, constant and one-finite-value numeric cases, with `sum = NA` whenever no finite observations exist.
- Categorical values containing declared observed levels, a declared zero-count level, an unexpected value, a sentinel, ordinary `NA` and literal `"NA"`, with exact `p_total` and `p_observed` fractions.
- A factor with an unused factor level that is absent from the specification, proving factor metadata alone does not create a result row.
- Text containing an empty string, whitespace-only string, ordinary text, a sentinel and ordinary missingness.
- Date and datetime examples with independently known minima, quartiles, maxima, timezone representation and ranges, plus invalid non-missing temporal values that must be skipped.
- Missing required and optional variables with unavailable counts and distinct reasons.
- A dictionary-derived specification whose order, levels and missing codes survive conversion into the canonical summary.

Use official R documentation for `quantile()`, `var()`, `sd()` and `shapiro.test()` and the installed `e1071` documentation for skewness and kurtosis. Record exact conventions, parameters and package versions in the spec review. If external evidence is unavailable for a field, state that limitation instead of treating current output as truth.

### 5. Implement the canonical builder

Make the smallest coherent refactor described by spec 010:

1. Replace the version dispatcher with one unversioned internal typed-summary builder.
2. Remove legacy v1-only builders, empty-table helpers and version-suffixed internal names that no longer have callers.
3. Preserve shared numeric, categorical, text and temporal cores where they meet the approved semantic policies; change them only for failing independently justified tests.
4. Build the `variables` audit row and exactly one successful component or skipped row for every specification row in specification order.
5. Preserve stable column types in every zero-row component.
6. Route typed `epi_stats_summary()` through the canonical builder without changing unrelated current-mode outputs.
7. Remove `summary_version` from `epi_eda_profile_summaries()`, `epi_eda_run()` and `epi_eda_render_report()` and update every repository caller in the same change.
8. Have `epi_eda_run()` write the returned components directly without recalculation or manual transcription.
9. Have the report render the returned object, present coverage and skipped reasons, identify empty components, label denominators and finite exclusions, and state temporal units.
10. Keep database inventory, catalogue profiling, correlation, contingency, outcome, multivariable and unrelated plotting behaviour outside this refactor.

Do not add dependencies, a summary class, a compatibility service or a second abstraction layer solely for presentation.

### 6. Update documentation and generated files

Update roxygen source, README, NEWS, the specification-first EDA vignette and the report template so active documentation describes only the canonical contract. Remove active `summary_version`, v1 and v2 interface references while retaining historical records under completed specs and review documents.

Edit roxygen comments in `R/` and regenerate `man/`; do not edit Rd or `NAMESPACE` by hand. Keep British English spelling and do not hard-wrap prose.

Document the intentional breaking return-shape change, removal of `summary_version`, the no-observed-total correction and the absence of a legacy adapter. Do not imply that a tag or release is created by this work.

### 7. Verify behaviour and inspect artifacts

Run changed test files first, then the spec acceptance commands:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-summaries|run_eda|eda_report|db-dictionary', reporter = 'summary')"
scripts/check-local.sh
scripts/check-cran.sh
git diff --check
```

Inspect more than exit codes:

- Compare every canonical in-memory component with the independent test calculations.
- Read all six written CSV files back and reconcile values, missing encodings, column names and types where CSV permits.
- Open the exact rendered HTML report and inspect coverage, skipped reasons, tables, plots, denominator explanations, units, empty sections, clipping and navigation.
- Inspect the source tarball and confirm that planning, checklists, outputs, secrets and local state are excluded as intended.
- Check `git status`, full diffs and generated documentation for unrelated changes or disclosures.
- Record all warnings, skips, check notes and unverified items in the spec review.

Do not accept regenerated fixtures or snapshots merely because they match current production output.

### 8. Review, recovery and completion

Review the change using findings-first severity ordering. Confirm the authorised breaking surface, callers, tests, documentation, CSVs and report together. State whether review was independent.

If a semantic ambiguity could change results, stop before altering production behaviour, record the question in the spec review and obtain a decision. If checks fail, fix the cause within spec scope or report the blocker; do not weaken acceptance criteria.

Keep implementation commits focused. Before committing, inspect `git diff --staged`. Do not create a tag, release or public pull request unless separately requested.

Mark spec 010 complete only when every acceptance item is evidenced, the review records exact commands and inspected artifacts, `future/README.md`, `future/TODOs.md`, `future/changelog.md` and the spec manifest agree on status, and no unresolved in-scope failure remains.

## Remaining Findings After Spec 010

The following work must not be folded into spec 010. Promote one coherent work package at a time into the next available numbered spec after checking `future/specs/` and `future/TODOs.md`.

### 1. Contingency truth contract

Priority: Highest remaining analytical validation risk after spec 010.
Truth-review IDs: `TR-022`, `TR-023`, `TR-024`.

Required planning decisions:

- Exact inclusion and missingness rules for 2x2 and NxN tables.
- Whether missing values are excluded or represented as levels.
- Eligible dependent and independent column types and minimum level counts.
- Exact Fisher versus chi-square method-selection policy, including sparse tables and whether a user request can be overridden.
- Returned estimates, p-values, method labels and aggregation behaviour.
- Arbitrary outcome labels rather than forced `Yes`/`No` categories.

Independent evidence must include small hand-authored tables with exact cells and denominators plus known Fisher and chi-square results from an independently documented method. Structural checks, totals summing to the source row count and p-value-column presence are insufficient.

Potentially affected public surfaces include `epi_stats_contingency_2x2_df()`, `epi_stats_contingency_2x2_tables()`, `epi_stats_2x2_test()`, `epi_stats_2x2_all()`, `epi_stats_contingency_nxn()`, returned columns, warnings, failure behaviour, numerical values, tests, roxygen and NEWS.

### 2. External fixture provenance, licensing and regeneration policy

Priority: High because fixtures are public, clinically themed and used as independent evidence.
Related truth-review IDs: `TR-001` to `TR-004` and the Priority 1 fixture concern in `future/TODOs.md`.

Recommended contract:

- Keep pinned local fixtures for deterministic offline package tests; do not download external packages or data during ordinary tests or package checks.
- Treat `data-raw/test-fixtures/` as the explicit regeneration path, not a test dependency.
- Record source package, exact version, canonical source URL, source archive checksum, dataset identifier, licence or redistribution basis, extraction method, local fixture checksum and transformation/exclusion record.
- Verify the exact upstream object before overwriting a committed fixture.
- Keep expected analytical values independent of package code. A source-data regeneration script may serialize data, but it must not call production summary functions to create expected results.
- Add guardrails that fail visibly when fixture bytes or expected-output provenance change without an explicit reviewed update.
- Resolve the redistribution basis for `blood_storage.csv` from an authoritative source. If redistribution is not justified, replace it with a legally reusable fixture or a small neutral hand-authored fixture; do not silently retain uncertain clinical records.

Potentially affected files include fixture `SOURCE.md` records, the regeneration script, fixture guardrail tests and possibly the blood-storage fixture itself. Avoid network access in tests and avoid adding confidential or institution-owned data.

### 3. Package source-build and scaffold hygiene

Priority: High when preparing a clean CRAN-like release; otherwise separate from analytical changes.

Known check findings from the reviewed environment:

- Top-level `outputs/` entered the source package because `.Rbuildignore` did not exclude it.
- `inst/project-template/data/.gitkeep` and `inst/project-template/outputs/.gitkeep` produced a hidden-file NOTE in package checks.

Recommended repair:

- Add a precise top-level `outputs` exclusion to `.Rbuildignore` so internal truth-review artifacts do not enter source tarballs.
- Remove installed `.gitkeep` placeholders.
- Have `epi_eda_create_project()` explicitly create the required empty `data/` and `outputs/` directories after copying template files.
- Update scaffold tests to assert directory existence and user-facing files rather than installed placeholder files.
- Inspect the built tarball and generated scaffold and require the relevant package-check notes to disappear without removing intended template behaviour.

This work changes project-scaffold contents but need not change the public function signature. Keep it in a separate focused branch or spec because it is unrelated to canonical summary semantics.

### 4. Correlation, shape and normality references

Priority: Medium.
Truth-review IDs: `TR-012`, `TR-025`, `TR-026`.

Add fixed non-perfect reference examples for skewness, kurtosis, Shapiro-Wilk, Pearson and Spearman coefficients, p-values, pairwise missingness, triangle extraction, relabelling and heatmap coordinate mapping. Record method versions and parameters. Perfect `r = 1`, object shape and propagation through the same Hmisc result family are insufficient.

Production changes are contingent on failing independent expectations. Potential public effects include numerical values, p-values, labels, missingness handling and heatmap placement.

### 5. Temporal boundary contract

Priority: Medium.
Truth-review IDs: `TR-014`, `TR-027`.

Define and test temporal quantiles, ranges, most-common values, missing-date differences, month/year transitions, ISO parsing, local time, timezone and DST boundaries. Distinguish Date days from datetime seconds. Current simple endpoints and two month counts do not cover these boundaries.

Potentially affected surfaces include `epi_stats_dates()`, `epi_stats_dates_freq()`, temporal summary cores, EDA temporal output and date plots.

### 6. Reports, serialized output, survival and visual regression

Priority: Medium after spec 010 stabilises canonical EDA output.
Truth-review IDs: remaining portions of `TR-015`, `TR-017` to `TR-020` and `TR-039`.

Spec 010 must validate its own canonical CSV and report paths, but broader package evidence remains needed for exact plot content, old plotting wrappers, enabled visual-regression strategy and Kaplan-Meier conversion/coordinates. Do not regenerate visual snapshots from current output and call them truth. Define plot data and visual contracts first, reconcile layer data numerically, then decide whether to re-enable, replace or remove obsolete snapshots.

For survival behaviour, use a fixed small example with independently specified survival estimates, risk sets, censor counts, strata parsing, confidence limits and step coordinates.

### 7. Helper-function semantic contracts

Priority: Medium or low depending on active user need.
Truth-review IDs: `TR-029`, `TR-034` to `TR-038`.

Separate candidate decisions include:

- `epi_stats_tidy()`: reconcile caller-supplied `perc_n` with the eligible population after exclusions.
- `epi_read()`: define dates, datetimes, leading-zero identifiers, mixed columns, locale, sampling-dependent type inference and default missing strings.
- Automatic factor conversion: define whether low cardinality alone justifies categorical conversion and protect Date/POSIX classes.
- `epi_clean_get_dups()`: decide whether repeated missing identifiers count as duplicates.
- CURP parsing: cite an authoritative specification and define century boundaries, invalid input and vector behaviour.
- `epi_sub_sample()`: define missing outcome strata, small-stratum rounding, replacement and exact eligible populations.

These choices can change returned rows, types, dates or sample sizes. Create one narrowly scoped spec for a demonstrated current need rather than combining all helpers into a general cleanup.

## Areas With Adequate Existing Evidence

Do not spend effort rewriting these solely to satisfy the new instructions unless a new requirement or defect appears:

- Pinned raw fixture origin for the verified upstream objects, subject to the unresolved blood-storage redistribution documentation.
- Ordinary and configured-sentinel missingness for the covered fixtures.
- Existing hand-anchored compact numeric/categorical cases that remain useful historical evidence, even though spec 010 will remove the compact public path.
- Character, factor and text count mechanics for the tested small fixtures.
- Ordinary row-wise and column-wise NA counts and percentages.
- Lookup recoding mechanics for the tested explicit mappings.
- Synthetic EDA names, row counts, declared levels, integer bounds, zero-row behaviour and deterministic seeds, while retaining the documented restriction that synthetic output is not evidence for inference or disclosure control.

Adequate evidence is limited to the tested contract. Do not generalise it to untested clinical meaning, external dictionaries or distributional fidelity.

## Cross-Cutting Impact Summary

### Public APIs and outputs that may change

- Spec 010 intentionally changes the EDA summary return shape and removes three public `summary_version` arguments.
- Later contingency work may change exact cells, exclusions, method selection and p-values.
- Later import, duplicate, CURP and sampling contracts may change inferred types, returned rows, derived dates and sample sizes.
- Canonical CSV names and components become the only EDA machine-readable summary output under spec 010.
- Rendered EDA report sections and explanatory text change to expose coverage, skips, denominators, finite exclusions and temporal units.
- Project scaffolds may stop containing `.gitkeep` files while retaining empty `data/` and `outputs/` directories.

### Tests and fixtures that may change

- Replace version-oriented EDA summary tests with unversioned canonical behaviour tests.
- Add hand-authored expectations before implementation and preserve fixture provenance.
- Retain external fixture data unless a separately approved provenance/licensing decision requires replacement.
- Do not regenerate expected values or snapshots from the production path under test.
- Preserve unrelated current-mode `epi_stats_summary()` regressions and all released interfaces outside authorised scopes.

### Documentation that may change

- Roxygen and generated Rd for changed public formals and return values.
- README, NEWS, EDA vignette and report template for the canonical contract.
- Truth-review catalogue and repair records after implementation evidence is stable.
- Fixture source records for exact provenance, checksums, licences and transformations.

### Dependencies and infrastructure

No new dependency is currently justified for spec 010 or the known build-hygiene repair. Independent reference work should prefer base R, official documentation and already declared packages. Add a dependency only when a numbered spec identifies a current requirement that cannot be met safely with existing tools.

## Completion Standard for Any Work Package

A work package is complete only when:

- Its numbered spec and acceptance criteria were approved before package-code changes.
- Required behaviour and semantic decisions are explicit and linked to authoritative or independently justified evidence.
- Expected values were authored independently of production logic.
- Focused tests and realistic invocations pass.
- Broader package tests, lint and the relevant local or CRAN-like checks pass, or every failure is reported and resolved.
- Exact returned objects, written files and rendered artifacts were inspected where applicable.
- Roxygen, generated documentation, README, NEWS, vignettes and schemas agree with observed behaviour.
- Public compatibility effects and intentional breaks are documented.
- Applicable checklist results, limitations and unverified items are recorded in the spec review.
- Worktree and staged diffs contain only intended changes and no confidential, personal or machine-specific information.
- No tag, release, publication or unrelated Git operation occurred without explicit authorisation.

## Handoff Summary for the Next Agent

Start by confirming current repository state and reading spec 010. Do not implement from the stale truth-review catalogue alone. Obtain human approval for spec 010, update its review to route all six checklists, record a clean baseline, write independent canonical-summary expectations, implement only the approved canonical path, inspect in-memory/CSV/HTML/tarball outputs and close the spec with evidence. After spec 010, promote the contingency truth contract as the default next analytical spec unless a human release-readiness priority selects the isolated source-build/scaffold hygiene repair first. Keep fixture provenance/licensing as a separate high-priority decision and keep all other truth gaps in separate, demonstrated scopes.
