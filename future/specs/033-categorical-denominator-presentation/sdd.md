# Software Design

Spec ID: `033-categorical-denominator-presentation`
Status: Review

## Public Calculation

Add `epi_eda_categorical_display(result, basis = c("compatibility", "column", "row", "overall"))`. `result` accepts either the named canonical list returned by `epi_eda_profile_summaries()` or an `epi_eda_stratified` result. It never accepts a data frame of observations, specification or database source and writes no files.

The fixed returned schema is:

1. `variable_order`
2. `level_order`
3. `name`
4. `label`
5. `type`
6. `level`
7. `group_id`
8. `group_order`
9. `group_label`
10. `is_overall`
11. `group_n`
12. `population_n`
13. `numerator`
14. `denominator`
15. `proportion`
16. `percentage_basis`
17. `denominator_scope`
18. `missing_treatment`
19. `is_missing_level`

Canonical input is represented as one `.overall` group. Standard missing values and declared missing codes are combined into one final missing-level row for every summarised categorical/binary variable; the function does not infer a not-applicable category. Stratified input preserves variable, group and declared/unexpected level order, including empty declared groups and zero-count levels.

The function validates component schemas, one-to-one group/variable identities, integer-like non-negative counts and these invariants before calculating: ordinary level counts sum to `n_observed`, the missing row equals `group_n - n_observed`, groups partition `population_n` when Overall is excluded, and Overall agrees with the included population when present. Corrupt aggregate inputs fail with concise value-free errors.

## Percentage Bases

| `basis` | Cell denominator | Missing treatment | Availability |
| --- | --- | --- | --- |
| `compatibility` | `n_observed` for an ordinary level; `group_n` for the missing level | Missing is excluded from ordinary denominators and shown separately over all group rows. | Canonical and stratified; default. |
| `column` | `group_n` for every level in the group | Missing is one included level; the full set sums to one when `group_n > 0`. | Canonical and stratified. |
| `row` | Sum of the same level's numerators across non-Overall strata | Missing is a separate level with its own cross-stratum denominator. Overall never contributes to the denominator, but an Overall display cell remains and therefore equals one when its count is positive and groups reconcile. | Stratified only. |
| `overall` | Included analysis population for every cell | Missing is one included level within the population denominator. | Canonical and stratified. |

`denominator` remains the factual integer denominator, including zero. `proportion` is `NA_real_` when the denominator is zero and otherwise `numerator / denominator`; no warning is emitted. `percentage_basis`, `denominator_scope` and `missing_treatment` use documented stable character values rather than inferred prose.

For stratified results, `population_n` is `metadata$n_included`; for canonical results it is the reconciled source-row total. Row denominators include every non-Overall declared, unexpected and missing stratum because those groups partition the included analysis population. Empty groups contribute zero. Missing strata excluded by the caller remain outside `population_n` as already recorded by stratified metadata.

## Table 1

Extend `epi_eda_table1()` with `basis = c("compatibility", "column", "row", "overall")`. The default call and exact returned column schema remain compatible. Categorical cells take their numerator, denominator and proportion only from `epi_eda_categorical_display()`; Table 1 performs formatting but no parallel percentage calculation. The default `display`, ordering and notes remain unchanged. Opt-in bases change only categorical denominators, percentages and basis notes; numeric, text and temporal rows retain their established rules.

## Frequency Companions And Plots

Both data-frame and PostgreSQL categorical plot preparation normalise their existing canonical aggregate frequencies through the same display calculation using `basis = "compatibility"`. Ordinary plots remain count bars, use the same level ordering/collapse threshold and continue excluding missing observations.

Compact frequency rows retain the existing leading fields `level`, `count`, `display_order` and `remainder`, then add the applicable shared display fields. `count` is identical to `numerator`. A collapsed remainder sums the selected numerators, retains the common denominator, recalculates its proportion safely and records `is_missing_level = FALSE`. Empty companions retain the complete typed schema. `n_missing` remains in plot inventory, while `missing_treatment` states that missing observations are excluded from the plotted ordinary-level denominator.

## Data-frame And Intake Outputs

`epi_eda_run()` adds a `categorical_display` result component and writes `categorical_display.csv` when `output_dir` is supplied. Existing components and summary CSV schemas remain unchanged. `epi_eda_render_report()` displays this aggregate table before ordinary plots.

`epi_eda_intake_run()` adds a `categorical_display` result component and manifest-owned `categorical_display.csv`. It uses the canonical result when no stratum is requested and the stratified result when available; Table 1 and the HTML report use the same compatibility rows. Earlier blocked stages leave the component `NULL` and artifact `not_created`. The five-column manifest schema and atomic publication rules remain unchanged.

## PostgreSQL Delivery

PostgreSQL categorical plot preparation enriches already-returned canonical frequency rows inside the existing read-only repeatable-read snapshot. It issues no SQL and collects no observations. `layout = "bundle"` retains its current result, file set, schemas and `compact-plot-data-1` metadata. `layout = "delivery"` writes enriched frequency companions and records `compact-plot-data-2` in the existing run-metadata field without adding a column.

The database report reads manifest-owned frequency companions only after bundle validation and displays their numeric fields. The renderer continues to accept valid `compact-plot-data-1` bundles: it reconstructs missing presentation fields in memory from validated canonical summary/variable tables and the retained compact counts, without changing the source bundle unless the caller explicitly republishes the owned report. New malformed companion fields fail value-free validation.

## Reports And Documentation

Data-frame, intake and PostgreSQL delivery HTML reports state the active compatibility basis, show numerator, denominator and proportion as machine-readable aggregate cells and explain that declared missing codes and standard missing values share one missing level. Reports do not infer a preferred basis or add governance language. Direct bar annotations remain absent because counts and percentages are available in linked companions without crowding figures.

README, NEWS, the specification-first EDA vignette and database walkthrough document the public function, four bases, zero-denominator behaviour, additive output artifacts and legacy delivery compatibility.

## Compatibility

- Existing canonical and stratified component schemas do not change.
- Existing `epi_eda_table1(result)` output names, ordering and display strings remain exact.
- Existing plots retain count scales, categories, collapse behaviour and missing exclusion.
- Existing data-frame runner/intake returns gain one named component and their written bundles gain one aggregate CSV; existing components and file schemas are unchanged.
- Default flat PostgreSQL bundles remain exact. Delivery frequency CSVs gain additive columns and a versioned contract value; valid prior delivery bundles remain renderable.
- The manifest remains exactly `artifact`, `type`, `path`, `status` and `checksum_md5`.

## Failure And Recovery

Invalid basis values, canonical/stratified component corruption, count reconciliation failures, negative or non-integer counts and unsupported row basis for canonical input fail before publication with value-free errors. Report and bundle failures retain existing staged atomic recovery. No operation mutates its input result.
