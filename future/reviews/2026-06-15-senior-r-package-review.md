# Senior R Package Review

Date: 2026-06-15

Prompt: `future/prompts/senior-r-package-review.md`

## A. Executive Summary

Overall judgement: mostly ready, with focused revision needed before treating the current development branch as a long-term stable base.

Main risks:

- The public API is broad and includes low-level tidy-eval re-exports that make the namespace harder to maintain.
- EDA summary tests currently duplicate implementation logic, so they may not catch real analytical mistakes.
- The EDA MVP is clear, but several edge cases around missing data, zero-row data, categorical denominators and large data are documented only lightly or not tested.
- Optional dependency handling is inconsistent: several functions guard suggested packages at runtime, while others call suggested packages directly.

Highest-value fixes:

- Tighten the exported API and avoid exporting implementation-adjacent helpers unless they are intentional user-facing functions.
- Replace circular EDA expected-output tests with committed fixture outputs or hand-computed expectations.
- Add edge-case tests for all-missing numeric EDA columns, zero-row EDA data, categorical percentages and missing-code semantics.
- Run `scripts/check-local.sh` and `scripts/check-cran.sh` after the review follow-up changes.

Confirmed small correction made during this review:

- Fixed the `epi_stats_numeric()` roxygen kurtosis wording from `lepto ~<3` to `lepto ~>3` in `R/epi_stats_numeric.R`.

## B. Findings Table

| Area | Severity | Location | Issue | Why it matters | Suggested fix |
| --- | --- | --- | --- | --- | --- |
| Public API stability | major | `R/utils-tidy-eval.R:33-42`, `NAMESPACE:3-11`, `NAMESPACE:96-104` | The package re-exports many `rlang` helpers and symbols (`quo`, `enquo`, `.data`, `:=`, etc.). | These exports widen the public API beyond episcout's domain and increase compatibility surface with little package-specific value. | Decide which, if any, tidy-eval helpers are intentionally public. Prefer importing internally and removing unnecessary re-exports in a scoped API cleanup. |
| Test quality | major | `tests/testthat/test-eda_summaries-fixtures.R:10-49`, `R/eda_summaries.R:51-98` | Fixture tests compute expected numeric and categorical summaries with nearly the same logic as implementation. | Duplicated implementation logic can let the same bug exist in both code and tests. | Store reviewed expected CSV outputs or use simpler hand-computed expectations, following the anti-circularity rule in `future/specs/002-penguins-raw-fixture/sdd.md:42-46`. |
| Statistical correctness | major | `R/eda_summaries.R:51-63` | All-missing numeric variables use `mean(..., na.rm = TRUE)`, `min(..., na.rm = TRUE)` and `max(..., na.rm = TRUE)` directly. | All-missing columns can produce `NaN`, `Inf`, `-Inf` and warnings in machine-readable summaries. | Reuse the stable empty/all-missing handling pattern from `epi_stats_numeric()` or add local safe scalar helpers with explicit tests. |
| User-facing semantics | major | `R/eda_missing.R:17-35`, `README.md:124-131` | The specification includes `missing_codes`, but missingness profiling only counts `NA`. | Epidemiological dictionaries often encode missing values as sentinel codes; ignoring them can understate missingness. | Clarify that `missing_codes` is reserved/not implemented or implement missing-code parsing under a numbered spec with tests. |
| Data semantics | minor | `R/eda_schema.R:87-96` | Logical variables are reported as `binary`, while integer variables are always reported as `numeric`. | A spec declaring integer or binary can look type-compatible or incompatible in surprising ways. | Define and test schema compatibility rules for `integer`, `numeric`, `binary` and labelled/factor variables. |
| EDA output interpretation | minor | `R/eda_summaries.R:82-94` | Categorical percentages divide by total row count, including missing values, without documenting whether the denominator is all rows or observed rows. | Both denominators are defensible, but users need predictable interpretation. | Document the denominator and add tests for categories with missing values. Consider including both `p_total` and `p_observed` if needed. |
| Dependency policy | minor | `R/epi_plot_parallel.R:67-74`, `DESCRIPTION:51-53` | `epi_plot_parallel()` calls `future::plan()` while `future` is only in `Suggests`; only `foreach` is explicitly checked. | If `future` is absent, the error comes from namespace lookup rather than the package's dependency guard. | Add `check_suggests(c("future", "doFuture", "foreach"))` or equivalent runtime checks before using suggested parallel packages. |
| CRAN check risk | minor | `R/epi_plot_parallel.R:66-71`, `R/epi_plot_parallel.R:123-128` | Parallel functions change the global future plan and then reset it. | Global plan mutation can surprise users and CRAN examples/tests if not tightly controlled. | Keep examples under `dontrun`, document the side effect, and test that the plan is restored. |
| Documentation accuracy | minor | `R/epi_stats_numeric.R:45-57` | Kurtosis interpretation text incorrectly described leptokurtic values as `<3`. | This is a statistical guidance error in a public helper. | Fixed in this review; regenerate Rd documentation before release. |
| Maintainability | note | `R/check_suggests.R:9-14`, `tests/testthat/test-check-suggests.R:1-6` | `check_suggests()` is internal but has roxygen docs and tests through `:::`. | This is acceptable, but it creates generated internal documentation and exposes internal naming to tests. | Keep if useful, but consider moving tests to user-facing functions that exercise missing suggested dependency paths. |

## C. Test Gaps

- `epi_eda_profile_summaries()`: all-missing numeric columns, zero-row data, non-numeric data under numeric spec, categorical levels with no observations, and missing categorical values.
- `epi_eda_profile_missing()`: zero-row data and specification `missing_codes` behaviour.
- `epi_eda_check_schema()`: integer versus numeric compatibility, binary/logical compatibility, factor versus character categorical variables, and date/datetime coercion expectations.
- `epi_eda_profile_plots()`: date histograms, high-cardinality text variables, non-syntactic column names, and missing `ggplot2` behaviour.
- `epi_plot_parallel()` and `epi_plot_save_parallel()`: missing suggested package errors and restoration of the caller's future plan.
- `epi_stats_numeric()`: non-numeric input rejection or coercion policy, infinite values, and `na.rm = FALSE` behaviour.

## D. Statistical Concerns

Confirmed issues:

- `epi_stats_numeric()` contained incorrect kurtosis guidance in roxygen text; this review corrected the source comment.
- `epi_eda_profile_summaries()` can emit non-finite numeric summaries for all-missing numeric variables.
- `epi_eda_profile_missing()` does not use dictionary `missing_codes`, despite the README quickstart showing that column as part of the expected specification.

Assumptions needing clarification:

- Whether categorical percentages should use all rows or observed non-missing rows as the denominator.
- Whether schema validation should treat integer columns as compatible with numeric specs and `0/1` integer columns as compatible with binary specs.
- Whether synthetic data should intentionally ignore distributional assumptions and generate only structurally valid placeholder values.

## E. Maintainability Concerns

- The namespace is broader than the package story. Re-exporting general tidy-eval helpers makes `episcout` look like a programming helper package as well as an epidemiology EDA package.
- The EDA code is readable and simple, but tests should assert independent contracts rather than restating the implementation.
- Optional dependency checks should use one consistent helper pattern so missing package errors remain predictable.
- The current `future/` workflow is useful, but review-derived work should be promoted to a numbered SDD/TDD spec before package-code changes.

## F. Suggested Next Commits

1. Regenerate documentation after the kurtosis wording fix.
2. Add `004-senior-review-followups` with SDD/TDD scope for API cleanup, EDA edge cases and dependency guards.
3. Replace circular EDA summary fixture tests with committed expected outputs.
4. Harden `epi_eda_profile_summaries()` for all-missing numeric columns and document categorical percentage denominators.
5. Clarify or implement `missing_codes` semantics in the EDA missingness workflow.
6. Tighten optional dependency checks for plotting, reporting and parallel helpers.
7. Run `scripts/check-local.sh` and `scripts/check-cran.sh`.
