# Software Design

Spec ID: `014-stratified-descriptive-summaries`
Status: Completed

## Public APIs

```r
epi_eda_profile_stratified(
  data, spec, strata,
  include_overall = TRUE,
  include_missing_stratum = TRUE
)

epi_eda_table1(result)
```

`strata` is exactly one non-missing character column name in v1. Boolean options are scalar non-missing logicals. Calculation returns class `c("epi_eda_stratified", "list")` with fixed components `groups`, `variables`, `numeric`, `categorical`, `text`, `temporal`, `skipped`, `metadata`. Table 1 accepts only this validated result and returns an ordinary long-form data frame.

## Validation

Validate data-frame/spec inputs through existing entry points and reject duplicate, empty or ambiguous names. The strata must be present in data and spec and declared categorical/binary. Its observed storage must be usable by canonical categorical semantics; otherwise the error points to `epi_eda_prepare()`. Do not coerce. Incompatible analysis variables remain explicit per-group skipped rows. Roles exactly `id` or `identifier` after trim/lowercase are policy-skipped, never inferred from names or uniqueness. Prepared POSIX objects are required for local datetime semantics; incompatible temporal values are skipped rather than interpreted under the machine timezone.

## Group Contract

Canonical missingness (`summary_missing_mask` plus the strata `missing_codes`) defines missing strata. Declared levels retain reviewed order and zero-count groups. Unexpected observed values follow in deterministic radix order and are flagged. A present missing group is last when included. Overall is first when requested.

Common group columns are `group_id`, `group_order`, `group_value`, `group_label`, `is_overall`, `is_missing_stratum`, `is_unexpected_stratum`. `groups` adds `is_declared_stratum` and `n`. IDs are stable `.overall`, `.stratum.001` etc., and `.missing`; flags disambiguate real labels such as Overall or Missing.

If missing strata are excluded, Overall summarizes the included population, not the full input. Thus non-overall group counts sum to `n_included`; `n_included + n_omitted_missing_stratum = n_input`; and every grouped component reconciles with Overall. Under the default, Overall is identical to canonical profiling of the full input.

Zero-row input returns Overall and declared zero groups as requested, no invented missing group, stable empty/NA summaries and zero denominators.

## Calculation Components

Every group subset preserves row order/classes and is passed to `build_typed_summaries()`. No statistical formula is duplicated.

- `variables`: common group fields plus canonical variable columns, one row per group × specification row.
- `numeric`: common group fields, `name`, `type`, `n`, `n_missing`, `n_observed`, `n_infinite`, then every canonical numeric field.
- `categorical`: common group fields, `name`, `type`, `level`, `n`, `n_total`, `n_observed`, `p_total`, `p_observed`, `is_declared`, `is_unexpected`, `is_missing_level`. It contains the population-wide union of declared and observed unexpected levels for every group, including zero cells, plus an explicit missing pseudo-level with `level = NA_character_`; literal `"NA"` remains distinct.
- `text`: common group fields plus canonical aggregate text diagnostics only; no values or examples.
- `temporal`: common group fields plus canonical temporal fields and explicit timezone/unit.
- `skipped`: common group fields plus `name`, `type`, `observed_class`, `reason`, including absent, incompatible, nested and identifier-policy exclusions. Extra data columns absent from spec appear once per group after spec rows.
- `metadata`: one row with `strata`, `strata_label`, both options, `n_input`, `n_included`, `n_omitted_missing_stratum`, `n_strata`, `summary_contract = "canonical-1"`, `stratified_contract = "stratified-1"`.

All proportions remain numeric. Zero observed denominators produce `NA_real_`, never zero. Overall values are calculated directly on the included rows, never as weighted displayed group values.

## Table 1

Return fixed columns: `variable_order`, `row_order`, `name`, `label`, `type`, `level`, `level_label`, `statistic`, `group_id`, `group_label`, `group_n`, `denominator`, `display`, `note`.

Numeric/integer variables produce `mean_sd`, `median_iqr`, and `missing` rows. Categorical/binary variables produce one row per ordinary and missing level using observed and total denominators respectively. Temporal variables produce `median_iqr`, `range`, and `missing`. Text remains aggregate-only with observed/unique, length, blank/whitespace and missing rows. The stratifier itself and policy-skipped identifiers are not repeated in the Table 1 body.

Unavailable values display as an em dash. Formatting is deterministic and derives only from calculation fields. Notes mark unexpected levels/strata, missing denominator conventions, infinite numeric exclusions and timezone/range units. No p-value column or inferential claim exists. Documentation warns that unsuppressed small cells are not disclosure-controlled.

## Privacy And Side Effects

No returned field, error or Table 1 cell contains raw text observations or examples. Categorical/group values are reviewed or observed categorical metadata and may still be sensitive. Neither function writes files, changes options/locales/timezones, mutates inputs, suppresses cells, or claims anonymisation.

## Recovery

Implementation is isolated on its feature branch. Stop if canonical reuse cannot preserve Overall equality, or if grouping would require implicit preparation. No dependency, tag or release is authorised.
