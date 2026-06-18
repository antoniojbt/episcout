# Software Design

Spec ID: `004-senior-review-followups`  
Status: Implemented  

## Scope

Implement the highest-value issues from the senior package review while keeping
changes small and easy to inspect:

- apply EDA `missing_codes` in missingness and summaries;
- make numeric summaries stable for all-missing and zero-row data;
- make categorical percentage denominators explicit;
- document descriptive schema behavior without redesigning it;
- add direct optional dependency guards where review found indirect errors.

## Public API

No new exported functions.

Existing outputs change as follows:

- `epi_eda_profile_missing()` counts standard `NA` and specification
  `missing_codes`.
- `epi_eda_profile_summaries()` excludes standard `NA` and specification
  `missing_codes` from numeric calculations and observed categorical counts.
- Categorical summaries keep `p` as the total-row denominator and add
  `p_observed` as the observed non-missing denominator.

Tidy-eval re-export cleanup is deferred to a later compatibility review and must
not be implemented in this spec.

## Inputs And Outputs

`missing_codes` rules:

- The column is optional.
- Values are semicolon-separated.
- Codes are trimmed with `trimws()`.
- Empty values mean no sentinel codes.
- Matching is by character representation of the source value.
- Standard `NA` is always missing.

Numeric summaries:

- `n` is total row count for the variable.
- `n_missing` counts standard `NA` and sentinel missing codes.
- `mean`, `sd`, `median`, `min` and `max` use only observed non-missing values.
- If no observed values remain, unavailable statistics are `NA_real_`.

Categorical summaries:

- Declared levels are retained even when the observed count is zero.
- `n` counts observed non-missing values matching the declared level.
- `p` is `n / total_rows`; for zero-row data it is `NA_real_`.
- `p_observed` is `n / observed_non_missing_rows`; for no observed values it is
  `NA_real_`.

## Data Flow

1. Validate `data` and `spec` with existing entry points.
2. For each specified variable, parse its `missing_codes`.
3. Build a missing mask using `is.na()` or character-code matching.
4. Compute missingness and summaries from the mask.
5. Return stable data frames with documented columns.

## Edge Cases

- Zero-row data returns `n = 0` and missing proportions as `NA_real_`.
- All-missing numeric data returns one stable row with summary statistics as
  `NA_real_`.
- Sentinel codes in numeric columns may be numeric or character values; matching
  is by character representation.
- Categorical levels with no observations remain in output.
- Missing variables keep existing error behavior in summaries and existing
  schema reporting behavior.
- Non-syntactic names continue to work through existing name-based indexing.

## Errors And Warnings

Errors should be clear, actionable and covered by tests.

- Missing suggested packages should fail through `check_suggests()` or an
  equivalent explicit guard.
- No warnings should be emitted for all-missing numeric summaries.

## Dependencies

No new dependencies.

Optional dependency guards should be explicit for reviewed plotting, reporting
and parallel paths. In particular, parallel plotting should check the optional
packages it uses before calling their namespaces.

## Deferred Work

- Public API cleanup for tidy-eval re-exports.
- Large-data backend support.
- Full schema compatibility pass/fail redesign.
