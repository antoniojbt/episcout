# Independent-Truth Validation Repair Plan

## Intended outcome

Correct the confirmed user-facing analytical defects, add independently anchored tests for the highest-priority truth gaps, and make schema, missingness, denominator and transformation behavior explicit without combining unrelated API redesigns. Existing behavior should remain compatible unless it is documented incorrectly, silently loses observations, ignores an explicit argument, or can return an analytically wrong result.

Success will be observed when each approved catalogue ID below has a discriminating behavior test derived independently of production logic, the corresponding implementation and documentation agree with the approved contract, compatibility decisions are recorded, and targeted tests, the full package suite, lint and CRAN-like checks pass without regenerating expected artifacts from the implementation under test.

## Approved findings

### Correctness and data-preservation repairs

- `TR-011`: `epi_stats_numeric(na.rm = FALSE)` currently ignores the explicit argument; only this `na.rm` defect is in the first repair scope, not the separately catalogued unverified shape/normality values.
- `TR-016`: EDA plots use raw sentinel codes even though the specification classifies them as missing.
- `TR-021`: the event numerator and population-at-risk denominator can describe different analysis windows.
- `TR-030`: transpose row labels are wrong when the identifier column is not first.
- `TR-031`: the documented full outer join defaults to left-only behavior and can drop right-only identifiers.
- `TR-032`: the repeated-measure long-to-wide path lacks a safe contract for zero/nonconsecutive visit codes, missing visits, duplicate identifier/visit pairs and unbalanced visits.

### Additive schema clarification

- `TR-002`: the generator guard proves absence of direct package calls but does not prove independent schema logic.
- `TR-003`: fixture semantic types and roles are repository-authored contracts, not externally validated truths.
- `TR-004`: expected schema fixtures mirror the production classifier.
- `TR-005`: current schema status establishes presence, not type conformance.

### Separate categorical-presentation review

- `TR-009`: declared-level counts and both denominators are independently anchored for the tested v1 cases.
- `TR-010`: v1 hides undeclared observed categories while retaining them in denominators, and the intended presentation has not been separately approved.

The labels `v1` and `v2` are not treated in this plan as an upgrade sequence. Spec 008 deliberately defined v1 as the historical compact numeric/categorical contract and v2 as a separate opt-in typed six-component contract (`future/specs/008-univariate-stats-eda-alignment/brief.md:13-18`, `future/specs/008-univariate-stats-eda-alignment/sdd.md:12-19`). Their purposes, names, defaults and long-term coexistence require a dedicated review before categorical presentation changes are implemented.

### Later independent-validation work packages

- `TR-012`, `TR-025` and `TR-026`: shape/normality values, non-perfect correlations, correlation p-values and correlation presentation transformations need independent numerical references.
- `TR-014` and `TR-027`: temporal quartiles, boundary behavior, missing date differences, month/year boundaries and plotted temporal values need independent fixtures.
- `TR-022`, `TR-023` and `TR-024`: 2x2/NxN cells, exclusions, denominators, method selection, p-values and reproducibility need their own contingency contract and reference-results work package.
- `TR-015`, `TR-017`, `TR-018`, `TR-019`, `TR-020` and `TR-039`: plot dispatch/content, rendered reports, orchestration, serialized outputs, visual regression and survival plotting need later end-to-end truth work after the first correctness repairs.

These later packages are part of the master sequence but are not to be folded into the first implementation change.

## Approved contract decisions

### Join behavior

`epi_clean_merge_nested_dfs()` will preserve every identifier present in any input by default. Full outer behavior is the default because the documented purpose is repeated-measure assembly and silent participant loss is unsafe. Explicit left-join behavior remains available by setting `all.x = TRUE, all.y = FALSE`. The public `all.x` argument is retained and an explicit `all.y` argument is added so legacy behavior is recoverable without relying on `...`.

### Analysis-window event proportion

`epi_stats_prop_outcome()` will first select rows whose population-at-risk variable equals `analysis_window`, then compute both numerator and denominator from that same eligible subset. Non-missing outcomes must be binary `0/1`; a missing outcome within the eligible subset, an invalid binary value, an absent column or a zero eligible denominator fails clearly rather than silently changing the population. The scalar return shape and rounding argument remain unchanged in the first repair to avoid unrelated API redesign.

### Schema presence and compatibility

The existing `status` field remains a presence status for compatibility. New fields will separately report `type_status` and `type_reason`, with statuses `compatible`, `coercible`, `incompatible` and `not_applicable`. The check remains descriptive and nonfatal by default; strict execution is deferred. The initial compatibility rules are: numeric accepts R numeric/integer storage; integer is compatible with integer storage and coercible from numeric only when all finite observed values are whole numbers; categorical accepts factor/character and may be coercible from declared coded levels; binary accepts logical and is coercible from values matching two declared levels; text accepts character and is coercible from factor; date accepts Date/IDate and is coercible from fully parseable ISO dates; datetime accepts POSIXct/POSIXlt and is coercible from fully parseable ISO-8601 values. Missing and unexpected variables receive `not_applicable`.

### Sentinel handling in plots

`epi_eda_profile_plots()` will apply the same specification-aware missing mask used by missingness and summary calculations before type conversion or plotting. Standard missing values and declared sentinel codes are omitted from analytical plots by default. An option to display sentinel codes is not part of the first repair; a later diagnostic-plot design may add one explicitly.

### Categorical levels and denominators

The cross-contract invariants are approved: declared levels remain visible at zero count; undeclared non-missing observed values must not disappear silently; `p_total` uses all source rows; `p_observed` uses all non-missing, non-sentinel observations; and rows plus missingness diagnostics must reconcile to their stated denominators. How each presentation exposes unexpected values is deferred to the dedicated v1/v2-neutral categorical review. Neither current contract becomes the default, preferred or deprecated contract through this repair plan.

## Changes

### Work package A — High-priority correctness repairs

#### `TR-021` — Window-specific event proportion

- Independent source: a hand-derived table containing events inside and outside the requested window, with numerator, denominator and proportion written explicitly before implementation.
- Affected paths: `R/epi_stats_prop_outcome.R`, `tests/testthat/test-missing-functions.R`, roxygen-generated `man/epi_stats_prop_outcome.Rd`, and `NEWS.md`.
- Expected behavior: only eligible-window events contribute; eligible `0` values remain in the denominator; the existing scalar value and rounding behavior remain stable for previously valid aligned inputs.
- Failure behavior: missing columns, non-binary outcomes, missing eligible outcomes and zero eligible rows produce specific errors.
- Source-to-output reconciliation: the test fixture will state the eligible row IDs, event count, denominator and exact expected proportion without calling another `episcout` function.

#### `TR-030` — Transpose labels

- Independent source: a hand-authored three-column table whose identifier column is first, middle and last, with the complete expected transposed table written literally.
- Affected paths: `R/epi_clean_transpose.R` and `tests/testthat/test-cleaning_functions.R`.
- Expected behavior: values from `id_col_num` become transposed column names and the first output column contains `colnames(df)[-id_col_num]` in the same order as the transposed data.
- Failure behavior: absent, non-scalar or out-of-range identifier-column selection fails clearly.
- Source-to-output reconciliation: compare the complete table, names and row-label column; do not use `str()` fragments or round-trip through production code.

#### `TR-031` — Full outer join

- Independent source: literal input tables with identifiers `{1,2}` and `{2,3}` and a literal expected `{1,2,3}` result, plus a separate legacy-left expectation.
- Affected paths: `R/epi_clean_merge_nested_dfs.R`, `tests/testthat/test-cleaning_functions.R`, roxygen-generated `man/epi_clean_merge_nested_dfs.Rd`, and `NEWS.md`.
- Expected behavior: default `all.x = TRUE, all.y = TRUE` preserves left-only, matched and right-only identifiers through two-table and multi-table merges; explicit `all.y = FALSE` retains legacy left behavior.
- Failure behavior: invalid join columns and fewer than two inputs continue to fail clearly; duplicate-key policy remains outside this function's first repair and is handled at the repeated-measure boundary below.
- Source-to-output reconciliation: assert the complete joined rows and values, not only dimensions, suffixes or class.

#### `TR-032` — Repeated-measure long-to-wide boundary

- Independent source: a hand-authored long table containing baseline code `0`, a nonconsecutive visit code, an unbalanced participant and a duplicate identifier/visit counterexample, with a literal expected named list and final wide table.
- Affected paths: `R/epi_clean_spread_repeated.R`, `R/epi_clean_merge_nested_dfs.R`, and `tests/testthat/test-cleaning_functions.R`.
- Expected behavior: visit values are used as list names rather than numeric list indices; zero and nonconsecutive codes work; unbalanced visits are retained by the full join; row/value association is preserved.
- Failure behavior: missing visit codes and duplicate identifier/visit pairs fail before reshaping because the function has no approved aggregation rule.
- Source-to-output reconciliation: compare complete per-visit tables and the final hand-authored wide result; no expected object may be produced by feeding one production transformation into another.

#### `TR-011` — `na.rm` semantics

- Independent source: a small numeric vector with one missing value and literal/base-stat expectations for `na.rm = TRUE` and `na.rm = FALSE`.
- Affected paths: `R/epi_stats_numeric.R`, `R/summary_cores.R` only if the core needs an explicit policy parameter, `tests/testthat/test-epi_stats_numeric.R`, roxygen-generated `man/epi_stats_numeric.Rd`, and `NEWS.md`.
- Expected behavior: `na.rm = TRUE` preserves current missing-exclusion behavior; with `na.rm = FALSE` and any missing observation, factual count fields remain populated while analytical location, dispersion, shape, normality and outlier result fields that depend on observed values return typed `NA` rather than silently using the reduced sample.
- Failure behavior: nonlogical or nonscalar `na.rm` fails clearly.
- Source-to-output reconciliation: expected values are literal or direct base-stat results on the transparent vector; no shared core call is used to generate them.

#### `TR-016` — Sentinel-aware EDA plots

- Independent source: small numeric, categorical and temporal fixtures containing ordinary missing values and declared sentinel codes, with explicit eligible value/count inventories.
- Affected paths: `R/eda_plots.R`, `tests/testthat/test-eda_plots-fixtures.R`, and relevant EDA documentation/vignette text.
- Expected behavior: sentinel codes are masked before histogram/bar construction or temporal coercion; plot titles/labels and nonmissing values otherwise remain unchanged.
- Failure behavior: an invalid declared temporal value that is not missing continues to fail through the documented temporal path; a declared sentinel must not trigger conversion failure.
- Source-to-output reconciliation: inspect `ggplot_build()` layer data and counts against hand expectations; ggplot class or geom type alone is insufficient.

### Work package B — Additive schema compatibility report

#### `TR-002`, `TR-003`, `TR-004`, `TR-005`

- Independent source: a purpose-built hand fixture covering every declared specification type and the `compatible`, `coercible`, `incompatible`, missing and unexpected branches. Expected classifications will be authored before the compatibility helper.
- Affected paths: `R/eda_schema.R`, `tests/testthat/test-eda_schema-fixtures.R`, `tests/testthat/test-penguins-raw-fixtures.R`, `data-raw/test-fixtures/make_external_fixtures.R`, both fixture `SOURCE.md` files, EDA documentation, generated Rd files, and `NEWS.md`.
- Expected behavior: existing presence fields and `status` remain stable; `type_status` and `type_reason` are appended and follow the approved compatibility matrix; the function reports but does not coerce or stop.
- Failure behavior: unsupported observed classes are `incompatible` with a reason rather than opaque class strings; missing and unexpected variables remain explicit.
- Source-to-output reconciliation: hand expectations establish correctness. Generated external-fixture schema CSVs remain integration/regression artifacts and will no longer be described as independent truth where their classifier mirrors production.
- Compatibility: appending columns is an additive schema change but may affect exact-column consumers; preserve existing columns and ordering, append new columns at the end, document the change, and test name-based consumers. No strict mode or automatic coercion is introduced in this work package.

### Work package C — Separate neutral review of categorical presentations

This work package is review-first and has its own approval gate. It must not be implemented as part of work packages A or B.

#### `TR-009`, `TR-010`

- Review the historical compact contract called v1 and the opt-in typed contract called v2 as two presentations, not as old/new or inferior/superior versions.
- Inventory their distinct use cases, fields, report/CSV consumers, empty-table behavior, unexpected-level representation, denominator semantics and variable-status behavior.
- Use neutral working names such as `compact` and `typed` during the review without renaming the public arguments yet.
- Build one transparent categorical truth matrix covering declared observed levels, declared zero-count levels, undeclared observed values, standard missing values, sentinel codes, literal `"NA"`, no declared levels, all-missing input and zero rows.
- Reconcile both presentations against the approved invariants: no silent unexpected-level loss, explicit total and observed denominators, retained zero-count declared levels, and auditable missingness.
- Decide separately whether both presentations remain indefinitely, whether their public names should gain descriptive aliases, whether v1 needs a warning or additional diagnostic output, and whether either default should ever change.
- Preserve the current default and both current public contracts until that review is accepted. Spec 008's completed compatibility decision is not reinterpreted as a migration commitment.
- If changes are approved after the review, create a separate implementation spec and map every change back to `TR-009` and `TR-010` plus any new review IDs. Do not regenerate expected categorical files from the production adapter.

### Work package D — Independent analytical references

After work packages A and B, create separate scoped specs rather than one broad refactor:

- Contingency (`TR-022`, `TR-023`, `TR-024`): hand-derived exact cells and denominators, independently implemented Fisher/chi-square results, explicit missing-level policy, arbitrary outcome labels and reproducible method selection.
- Correlation and shape (`TR-012`, `TR-025`, `TR-026`): fixed published or independently implemented reference values for skewness, kurtosis, Shapiro-Wilk, non-perfect Pearson/Spearman coefficients, p-values and exact triangle/label mappings.
- Temporal (`TR-014`, `TR-027`): explicit quartiles/ranges, missing-date policy, month/year boundaries, local time and DST cases, plus plot-layer value checks.
- EDA/report/survival integration (`TR-015`, `TR-017`, `TR-018`, `TR-019`, `TR-020`, `TR-039`): selected rendered cells, CSV value/type round trips, plot-layer data contracts, an enabled but non-self-authorizing visual review path, and a fixed survival reference example.

Each later spec requires its own contract acceptance before implementation if independent evidence exposes a behavior choice or breaking redesign.

## Validation

### Test construction rules

- Write the expected hand fixtures and reference results before changing production code.
- Do not calculate expected results through `episcout`, shared internal cores, snapshots generated from current output, or scripts that duplicate the production decision tree.
- Keep fixtures minimal and readable; use one fixture per coherent contract rather than numerous opaque files.
- For cross-implementation checks, document package/method versions, assumptions, missing-data policy, denominator policy and any randomness.
- Preserve confidential-data safeguards; public row-level clinical fixture values need not be reproduced in reports or failure messages.

### Targeted commands

Run the existing repo-local wrapper for every R command. Initial targeted validation should include the affected files, for example:

```bash
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-missing-functions.R', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-cleaning_functions.R', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-epi_stats_numeric.R', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-eda_plots-fixtures.R', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-eda_schema-fixtures.R', reporter = 'summary')"
```

### Broad commands

After documentation is regenerated and changed R files are styled, run:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::document()"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); stopifnot(length(findings) == 0L)"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); covr::report()"
```

### Observable success criteria

- Window-specific event tests fail on the current global-numerator behavior and pass only when numerator and denominator share the eligible window.
- Full-join tests prove preservation of left-only, matched and right-only identifiers; explicit legacy-left behavior is also tested.
- Transpose and repeated-measure tests compare complete literal outputs and fail on shifted labels, zero-index visits, missing visits and duplicate identifier/visit pairs as specified.
- `na.rm = FALSE` produces the approved typed missing analytical fields rather than the `na.rm = TRUE` result.
- Sentinel-coded values are absent from built plot layers but remain counted in missingness output.
- Schema tests independently exercise every compatibility status while existing presence columns remain stable.
- No v1/v2 categorical behavior or default changes before the separate review gate is accepted.
- No visual snapshot update is accepted as proof of numerical or semantic correctness.

## Compatibility and release handling

- Treat `TR-021`, `TR-030`, `TR-031`, `TR-032`, the `na.rm` portion of `TR-011`, and sentinel masking in `TR-016` as correctness repairs because the current behavior can return the wrong population, lose identifiers, mislabel data, ignore an explicit argument, fail on ordinary visit codes, or plot values declared missing.
- Preserve an explicit route to legacy left joins and document the default change prominently.
- Keep scalar event-proportion return shape and current function names in the first repair.
- Keep existing schema fields and order; append compatibility fields and avoid strict failure or automatic coercion in the additive schema work package.
- Do not change categorical defaults, rename v1/v2, deprecate either presentation or assume one supersedes the other until the separate categorical review is approved.
- Record user-visible result changes in `NEWS.md` and roxygen documentation, with before/after examples for join and event-window behavior.
- Do not combine these changes with unrelated style cleanup, function renaming or broad test refactors.

## Limitations

This plan does not claim that all 40 catalogue rows will become independently validated in one change. Work packages A and B address the approved correctness and schema decisions; work package C is a separate review; work package D orders the remaining high-value validation work. Lower-priority partial areas such as import inference, duplicate missing identifiers, CURP boundaries, stratified sampling edge cases and synthetic-distribution semantics (`TR-034` through `TR-040`, except `TR-039`) remain outside the first repair and must stay visible in the catalogue.

External authoritative truth is unavailable for some presentation and domain-policy choices. Where no authoritative source exists, small transparent hand-derived fixtures and explicit user-approved contracts are the truth source. The plan must be revised if implementation inspection reveals a materially broader compatibility surface than the paths listed above.
