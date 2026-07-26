# Review Notes

Spec ID: `007-eda-stats-alignment-review`
Status: Completed

## Executive Conclusion

`epi_stats_*` is the active main statistics layer and should remain central to the package. The specification-first EDA layer should be refactored to share type-specific statistical cores with that API, not remain a parallel implementation and not call broad presentation-oriented public outputs directly.

The review recommends a shared-core architecture, a richer complete EDA summary contract, separation of computation from formatting, and staged follow-up specs. The first implementation spec should cover common missingness/count primitives, univariate type cores and EDA integration. Correlation, contingency and outcome APIs should be reviewed under the same conventions but changed in later scoped specs.

## Correction of the Earlier Classification

No active README, API documentation, release note or architecture record calls `epi_stats_*` legacy or deprecated. The inaccurate word came from two uses of “older” in the former idea note: one compared the creation time of the APIs and one described `epi_stats_summary()`. That wording did not establish lifecycle status.

Repository evidence establishes the opposite:

- README lists `epi_stats_*` as the package Statistics feature and one of the two main usage paths.
- NAMESPACE exports 25 `epi_stats_*` functions.
- The archived repository audit says to keep existing helpers and add the specification-first layer above them.
- The archived EDA implementation instruction says to use existing `epi_stats_*` helpers where suitable.
- Current tests, vignettes and plotting helpers actively consume the statistics interfaces.

## Public Function Contract Matrix

The matrix includes all 25 `epi_stats_*` exports and the related exported `rename_contingency_2x2_cols()` helper found in the statistics source.

| Group | API and signature | Current input and output contract | Missing/edge behaviour | Dependencies, consumers and evidence | Confirmed concern | Recommended disposition |
| --- | --- | --- | --- | --- | --- | --- |
| Numeric | `epi_stats_numeric(num_vec = NULL, na.rm = TRUE, coef = 1.5, ...)` | Numeric vector to one-row base data frame with 25 count, location, dispersion, shape, normality and outlier fields | Empty/all-NA are stable; non-numeric input raises an opaque base error; non-finite input can return `NaN`, infinities and internally inconsistent fields | Imports `e1071` and `stats`; calls `epi_stats_count_outliers()`; used by `epi_stats_summary()`; dedicated tests and README/vignette examples | EDA independently computes only seven of its fields; finite-value and validation policy are incomplete | Retain as the main numeric public API; move calculations into a typed numeric core shared with EDA; standardise names and non-finite policy in an approved migration |
| Numeric | `epi_stats_count_outliers(num_vec = NULL, coef = 1.5, ...)` | Numeric vector to scalar outlier count using Tukey fences | Empty/all-NA return `0L`; negative coefficient errors; non-finite policy is unspecified | Uses `stats::quantile`; called by `epi_stats_numeric()`; dedicated tests | Numeric summary separately recomputes quartiles/fences, risking drift | Retain public wrapper over the same numeric/outlier core used by `epi_stats_numeric()` |
| Missingness | `epi_stats_na_perc(df = NULL, margin = 2)` | Data frame-like input to base data frame with `na_counts` and percentage by row or column, with identifiers in row names | Mixed types and zero denominators are stable; counts only standard `NA`; `NULL` becomes an empty data frame | Base R only; dedicated tests; no EDA caller | Output names, percentages and row-name identifiers differ from EDA; no sentinel-code input | Retain active standalone API but share common count/proportion primitives; do not force specification semantics into the generic wrapper |
| Character | `epi_stats_chars(df)` | Character/all-NA columns to tibble with one row per variable and missing, completeness, length, empty, unique and whitespace fields | All-NA is handled; zero-row character columns return a zero-row table rather than a variable row; coercion policy for non-character values is implicit | Uses dplyr, tidyr and stringr; dedicated tests | EDA `text` variables receive no summary even though this API implements the required MVP fields | Retain and rebuild over a text core; EDA adapter should attach spec metadata and sentinel handling |
| Factor | `epi_stats_factors(df)` | Factor columns to tibble with missingness, completeness, ordered flag, unique count and a formatted top-three string | Zero-row factor produces `NaN` completeness and malformed `" ()"`; unused levels are absent | Uses dplyr, purrr and tibble; dedicated tests | `top_counts` is presentation text rather than machine-readable data; overlaps `epi_stats_fct_table()` and EDA categorical summaries | Retain during migration; replace internals with categorical core and consider deprecating formatted `top_counts` after a machine-readable replacement exists |
| Factor | `epi_stats_fct_table(df, vars_list = NULL)` | Factor/character columns to long tibble `variable`, `level`, `count`; explicit missing level becomes string `"NA"` | Drops declared unused factor levels; missing and literal `"NA"` become ambiguous; selection errors are delegated to dplyr | Uses tibble, dplyr, purrr and tidyr; dedicated tests | Closest public contract to EDA categorical output but lacks total/observed denominators and specification levels | Retain as categorical count table adapter over shared core; distinguish missing from literal levels and support zero-count levels through the core |
| Date | `epi_stats_dates(date_vector)` | Date/IDate vector to 11-row base data frame with `Statistic` and character-like `Value` | All-missing and zero-length inputs warn then error; POSIXct is rejected; mixed value types are collapsed to character | Uses stats and dplyr; called by `epi_stats_dates_multi()`; tests and vignette cover ordinary Date/IDate only | EDA date/datetime variables receive no summaries; output is presentation-oriented and loses types | Redesign around a typed temporal core supporting Date/IDate/POSIXct/POSIXlt; retain wrapper only with a documented migration |
| Date | `epi_stats_dates_multi(df)` | Selects Date columns and returns one wide tibble row per column | No Date columns return an empty bind; datetime columns are excluded; inherits `epi_stats_dates()` failures | Uses lubridate, dplyr and tidyr; calls `epi_stats_dates()`; dedicated tests | Direct orchestration overlaps future EDA temporal dispatch but lacks specification awareness | Retain as dataframe adapter over temporal core; support documented temporal classes and stable zero-column output |
| Date | `epi_stats_dates_freq(date_vector)` | Date/IDate vector to list of consecutive day differences and year-month frequency table | Rejects datetime; missing-value ordering/frequency policy is not explicit | Base/stats behaviour; dedicated ordinary-input tests | Frequency analysis is distinct from descriptive summary and should not be forced into default EDA output | Retain as a separate temporal-analysis helper; share validation/coercion only |
| Summary orchestration | `epi_stats_summary(df = NULL, codes = NULL, class_type = "chr_fct", action = "exclude")` | Selects numeric/integer or character/factor columns, includes/excludes codes, and returns a variable-bound tibble whose schema depends on mode | Uses expression/eval and `select_if()`; output varies by mode; no eligible/code values may produce empty tables; dates are excluded | Uses tibble, dplyr and purrr; calls `epi_stats_numeric()` in one path; substantial tests are positional and mode-specific | This should be the main dataframe summary orchestrator but currently cannot cover all EDA types or provide one stable composite contract | Redesign as explicit type dispatch over shared cores; permit a breaking replacement contract with a staged migration from current modes |
| Presentation | `epi_stats_tidy(sum_df = NULL, order_by = "percent", perc_n = NULL, digits = 2, decreasing = TRUE)` | Reshapes count output, assumes first column is identifier, adds row sums/percent and sorts | Missing `perc_n` errors; zero denominator returns `Inf`; missing columns fail indirectly; `digits` is documented but unused | Uses tibble, tidyr and dplyr; consumes one `epi_stats_summary()` mode; tested for routine cases | Computation, reshaping and presentation are coupled to an unstable upstream schema | Deprecate in favour of explicit machine-readable summary outputs and a narrowly scoped presentation helper after replacement exists |
| Presentation | `epi_stats_format(df = NULL, skip = NULL, digits = 2, ...)` | Formats numeric columns as character strings and returns a base data frame | Documentation says `skip` is a string but implementation treats it as column positions; class conversion is intentional but easy to misuse | Uses base R plus `epi_clean_cond_numeric()`; consumes summary outputs; routine tests | Presentation conversion should not occur inside computational pipelines or EDA CSV generation | Retain as an explicit presentation utility after correcting its contract; keep outside shared statistical cores |
| Correlation | `epi_stats_corr(df = NULL, method = "spearman")` | Numeric data frame to list containing Hmisc `rcorr` result and full long correlation/p-value tibbles | Non-numeric columns coerce with warnings; fewer than five rows errors from dependency; constant columns return undefined correlations; method errors are delegated | Suggests Hmisc; uses dplyr/tidyr/tibble; consumed by correlation reshape and heatmap functions; tested on normal inputs | Validation, minimum sample and undefined-correlation policies are implicit | Retain as active multivariable API, separate from univariate EDA; add explicit validation and stable output contracts in a later correlation spec |
| Correlation | `epi_stats_corr_triangle(cormat = "cormat_all$cormat")` | Hmisc-like correlation object to lower-triangle data.tables with `Var1`, `Var2`, `value` | The default is a literal string and fails; malformed objects fail opaquely; empty behaviour is not specified | Suggests data.table; consumes `epi_stats_corr()` output; used by heatmap workflow and tests | Invalid default and object contract; output classes differ from upstream tibbles | Retain workflow capability but require the object argument, validate it and standardise output classes/names in the correlation spec |
| Correlation | `epi_stats_corr_rename(r_vals = <string>, p_vals = <string>, vars_list = vars_list, var_labels = var_labels, digits = 2)` | Long triangle tables to renamed/rounded list | Defaults are invalid literal/self-referential values; label lengths and required columns are unvalidated; empty tables return early | Base factor/round operations; consumed by heatmap workflow and tests with explicit inputs | Function combines relabelling and rounding with unusable defaults | Replace with a validated relabelling adapter; require inputs, separate numeric rounding from identity labels and migrate plotting consumers |
| Contingency | `epi_stats_contingency_2x2_df(df, x_var, y_var)` | Named/indexed columns to long frequency base data frame | Standard `table()` drops missing values; invalid names produce opaque indexing errors; dimensionality is not restricted to 2x2 | Base table; called by table-list wrapper; tested on valid categorical data | Name promises 2x2 but function accepts arbitrary cardinality | Retain capability under a clearer general contingency-table contract or enforce two levels; decide and migrate in contingency spec |
| Contingency | `epi_stats_contingency_2x2_tables(df, x_var)` | One target against every other column to named list of frequency data frames | Can build huge tables for continuous/high-cardinality variables; inherits missing/index errors | Calls `epi_stats_contingency_2x2_df()`; tests cover ordinary categorical data | No eligibility or size policy despite 2x2 naming | Replace internals with validated column selection and explicit cardinality limits; keep wrapper during migration |
| Contingency | `rename_contingency_2x2_cols(contingency_2x2_list, df, x_var)` | Renames second column in each table from list names | `df` and `x_var` are unused; malformed lists fail indirectly | Direct consumer of table-list output; exported and documented with contingency family | Non-prefixed public helper and redundant parameters expand compatibility surface | Introduce correctly prefixed replacement or remove need through canonical output names; stage deprecation because it is exported |
| Contingency | `epi_stats_contingency_2x2_test(df, target_var, other_var, test_type = "fisher.test")` | Two columns to one-row broom test result plus variable name | Automatically overrides requested test for dimensions/counts; Fisher call uses unseeded Monte Carlo with one million simulations; invalid variables/test shapes fail indirectly | Uses stats and broom; called by all-pairs wrapper and aliases; tested for ordinary cases | Naming, automatic selection and randomness are not reproducible or transparent | Redesign with explicit eligibility, test-selection result, deterministic seed/simulation policy and stable method metadata |
| Contingency | `epi_stats_contingency_2x2_cols(df, min_unique = 2)` | Returns names with at least `min_unique` values | Counts `NA` as unique and admits high-cardinality/continuous columns; `min_unique` is not validated | Base `sapply`; called by all-pairs wrapper and alias; routine tests | Does not actually select 2x2-compatible columns | Replace with explicit non-missing cardinality and class policy; preserve old wrapper only during migration |
| Contingency | `epi_stats_contingency_2x2_all(df, target_var, test_type = "fisher.test")` | Runs tests between target and all selected columns, returning bound tibble | Inherits selector, validation, randomness and test-selection issues | Uses dplyr and canonical test/selector; alias and tests exist | Aggregates unsafe eligibility decisions | Rebuild after canonical selector/test contracts are approved; keep separate from default univariate EDA |
| Contingency alias | `epi_stats_2x2_test(...)` | Forwards to canonical test | Inherits canonical behaviour | Direct compatibility alias; tests use it | Lifecycle is documented as backwards compatibility but no formal deprecation policy exists | Keep through migration, then deprecate only with replacement documentation |
| Contingency alias | `epi_stats_2x2_cols(...)` | Forwards to canonical selector | Inherits canonical behaviour | Direct compatibility alias; tests use it | Same as above | Keep through migration, then deprecate only with replacement documentation |
| Contingency alias | `epi_stats_2x2_all(...)` | Forwards to canonical all-pairs wrapper | Inherits canonical behaviour | Direct compatibility alias; tests use it | Same as above | Keep through migration, then deprecate only with replacement documentation |
| Contingency | `epi_stats_contingency_nxn(df, dep_var, ind_vars)` | Builds wide counts, totals and percentages for one outcome and one or more grouping variables | Validates column presence and empty rows; hard-codes extra `Yes`/`No` outcome levels; zero totals can produce undefined percentages | Uses stats, tidyr and dplyr; dedicated edge tests | Confirmed arbitrary outcomes receive spurious `Yes`/`No` columns; source filename also misspells contingency | Retain NxN summary capability but remove hard-coded labels, define denominator/missing policies and standardise output naming in contingency spec |
| Epidemiological outcome | `epi_stats_prop_outcome(df, outcome_var_window, pop_at_risk_var, analysis_window, round_dig = 4)` | Returns and prints a rounded scalar event proportion | Numerator counts outcomes across all rows while denominator filters the analysis window; missing/invalid columns are unvalidated; zero denominator returns `Inf` | Base R; isolated tests cover a favourable example | Confirmed numerator/denominator population mismatch is a correctness defect; printing is an unwanted side effect | High-priority bug-fix spec after contract approval; filter numerator and denominator to the same window, validate binary outcomes/denominator and return structured evidence without printing |

## EDA Specification-Type Map

| Specification type | Current EDA behaviour | Existing statistics capability | Target shared-core contract | Disposition |
| --- | --- | --- | --- | --- |
| `numeric` | Compact one-row summary with counts, mean, SD, median, min and max | `epi_stats_numeric()` and outlier helper provide richer fields | Common finite numeric core plus shared counts; EDA attaches spec metadata | Implement first in spec 008 |
| `integer` | Same as numeric | Same numeric API; no integer-specific count policy | Numeric core with preserved declared/observed type metadata | Implement first in spec 008 |
| `categorical` | Long declared/observed levels with counts and two denominators | Factor summary and factor table provide overlapping partial contracts | Machine-readable categorical core preserving declared zero levels and unexpected observed levels | Implement first in spec 008 |
| `binary` | Same as categorical | Categorical helpers and contingency functions exist, but inferential contingency is not a default univariate summary | Categorical core with validated two-level expectation and no automatic hypothesis test | Implement first in spec 008 |
| `text` | No summary and no skip row | `epi_stats_chars()` provides MVP-required metrics | Text core with missing/sentinel preprocessing and typed length/count output | Implement first in spec 008 |
| `date` | No summary and no skip row | `epi_stats_dates()`/multi provide partial presentation output | Temporal core with typed Date summaries and documented range units | Implement first in spec 008 |
| `datetime` | No summary and no skip row | No current `epi_stats_*` function supports POSIXct/POSIXlt | Temporal core preserving time zone and returning typed datetime summaries | Implement first in spec 008 |

A read-only seven-type probe confirmed that only numeric/integer and categorical/binary names appear in current EDA outputs; `text`, `date` and `datetime` are silently absent.

## Confirmed Behavioural Findings

- Full package tests pass, but passing tests do not cover all intended contracts.
- `epi_stats_numeric(c(1, Inf, -Inf, NA))` returns `NaN`, `Inf` and `-Inf` across fields while reporting zero outliers; a non-numeric vector fails with an opaque base error.
- Zero-row `epi_stats_factors()` returns `NaN` completeness and malformed `top_counts = " ()"`.
- `epi_stats_fct_table()` drops a declared but unobserved factor level.
- `epi_stats_dates()` warns and errors for all-missing and zero-length Date vectors, and rejects POSIXct.
- `epi_stats_tidy(..., perc_n = 0)` returns infinite percentages.
- Default calls to `epi_stats_corr_triangle()` and `epi_stats_corr_rename()` fail because their defaults are not real objects.
- Invalid contingency variable names produce opaque indexing errors, and the 2x2 column selector admits columns based only on a minimum unique count.
- `epi_stats_contingency_nxn()` adds `Yes` and `No` columns to outcomes whose actual labels are unrelated.
- `epi_stats_prop_outcome()` can count outcomes outside the requested analysis window and returns `Inf` for an empty risk set.

## Downstream Compatibility Matrix

| Consumer | Current dependency | Required migration evidence |
| --- | --- | --- |
| `epi_eda_profile_summaries()` | Two list components and compact schemas | Contract tests for all proposed components and explicit variable coverage |
| `epi_eda_run()` | Writes two summary CSV files | Stable file inventory, compatibility window and deterministic naming for new components |
| Bundled EDA report | Renders numeric and categorical tables only | Sections for variables, text and temporal summaries plus graceful empty tables |
| Specification-first vignette and README | Describe only numeric/categorical components | Updated examples and migration notes after implementation |
| Blood-storage and penguins fixtures | Expected numeric/categorical CSV outputs | Independent expected outputs retained for old contract during transition and extended for new types |
| `epi_stats_summary()` callers | Mode-dependent tibble shapes | Replacement high-level summary contract and compatibility tests |
| Heatmap functions | Correlation triangle list with `value` columns | Correlation output naming/class contract and plotting regression tests |
| Contingency aliases | Forward to canonical names | Staged deprecation tests and NEWS entries if removal is approved |

## Decisions

1. Treat `epi_stats_*` as active main code, not legacy code.
2. Use shared unexported computation cores with public statistics and EDA adapters.
3. Make specification-aware missing masking an EDA adapter responsibility built on shared basic count/proportion primitives.
4. Expand EDA to cover every specification type and add an explicit skipped/status table.
5. Separate computational results from display formatting and printing.
6. Keep correlation, contingency and outcome analysis out of default univariate EDA summaries, while reviewing them under the same validation and output principles.
7. Permit breaking redesign only through later scoped specs with migrations; spec 007 changes no package behaviour.

## Ordered Implementation Programme

### Provisional spec 008 — Univariate statistics cores and EDA alignment

- Define common counts/missingness and type-specific numeric, categorical, text and temporal cores.
- Refactor active public univariate wrappers and `epi_stats_summary()` adapters against approved target contracts.
- Expand EDA summaries, CSVs, report sections and fixtures for all seven specification types.
- Provide compatibility adapters or a staged transition for existing EDA numeric/categorical outputs.

### Later scoped spec — Correlation contracts

- Validate inputs and methods, repair invalid defaults, standardise long/triangle schemas and migrate heatmap consumers.

### Later scoped spec — Contingency contracts

- Separate general tables from exactly-2x2 eligibility, define missingness/cardinality/test-selection policy, make simulation reproducible and stage alias deprecations.

### High-priority focused bug fix — Outcome proportion

- Correct the numerator population, validate the risk set and remove printing. This may proceed as a small independent bug-fix spec if prioritised before the larger refactor.

### Later presentation cleanup

- Replace or deprecate `epi_stats_tidy()` after machine-readable summary replacements exist; correct and retain an explicitly presentation-only formatter if still useful.

## Risks

- A single implementation spec covering all groups would be too broad; the ordered programme deliberately isolates univariate EDA, correlation, contingency and outcome work.
- Adding richer EDA outputs can break file consumers unless old names remain during a compatibility window.
- Shared cores must not inherit presentation-oriented character coercion or optional-dependency requirements.
- Monte Carlo hypothesis tests require explicit reproducibility policy.
- Existing tests often assert positions and current shapes; future tests must assert intended behaviour independently.

## Open Questions

No questions block completion of this design review. Human acceptance is required before spec 008 is created, particularly for the proposed EDA component schema, compatibility window and public deprecation candidates.

## Closeout Notes

The review met its design-only scope. It inventories every exported statistics function, maps every EDA type, records baseline checks and confirmed probes, identifies downstream consumers, and recommends one architecture and an ordered builder programme. No package implementation is authorised by this closeout.
