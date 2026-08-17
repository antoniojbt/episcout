# Software design

`epi_eda_longitudinal()` accepts only a data frame or an unmodified reviewed PostgreSQL source. It validates the identifier representation, reviewed time universe and selected reviewed variables before constructing an object. `id` need not occur in the specification; `time` must be reviewed categorical/binary. A NULL variable selection follows specification order while excluding ID/time and identifier roles; an explicit unique selection preserves caller order. Required absent or incompatible columns fail before result construction.

Data-frame IDs are character/factor or exact finite integer-valued numeric values through `2^53 - 1`. PostgreSQL IDs are text, integral or UUID; textual comparison uses the established deterministic C-collated identity contract. The time universe is exactly the reviewed declared order (`FALSE`, `TRUE` for logical binary). A supplied order must match it exactly. Missing time remains structural and in the canonical Missing summary stratum; unexpected observed time is a hard error; declared empty occasions remain represented.

The fixed result components, in order, are:

```text
metadata, structure, followup, timepoints,
missingness, summaries, change, issues
```

Their containers are fixed: one-row `metadata`; one-row `structure`; `followup` list with `observation_count`, `first_observation`, `last_observation`, `gap_status`; `timepoints` data frame; `missingness` list with `by_time`, `entity_summary`, `usable_measurement_distribution`, `interior_missing`; canonical `epi_eda_stratified` `summaries`; `change` list with `first_to_last`, `adjacent`; and one `issues` data frame. Every custom table has stable typed zero-row output.

`metadata` columns are `contract_version`, `backend`, `id`, `time`, list-columns `resolved_variables` and `time_order`, specification/selected-specification/source SHA-256 fingerprints, `count_contract`, `count_maximum`, and `snapshot_mode`. The contract is `longitudinal-eda-1`, exact base-R doubles through `2^53 - 1`, with snapshot `caller-owned-in-memory` or PostgreSQL RRRO.

`structure` columns are exactly `n_rows`, `n_missing_id`, `n_blank_id`, `n_missing_time`, `n_invalid_id_and_missing_time`, `n_valid_panel_rows`, `n_valid_entities`, `n_declared_timepoints`, `n_observed_timepoints`, `n_observed_id_time_cells`, `n_duplicate_cells`, `n_duplicate_excess`, `max_rows_per_cell`, `n_entities_with_duplicate_cell`, `n_expected_cells`, `n_complete_entities`, `n_incomplete_entities`.

`followup` schemas are `(n_timepoints_observed,n_entities)`, `(time_index,timepoint,n_entities)` for first/last, and `(has_gap,n_entities)`. `timepoints` columns are `time_index`, `timepoint`, `n_rows`, `n_entities`, `n_first_observed`, `n_last_observed`, `n_retained`, `n_not_present_previous`, retention numerator/denominator/proportion and current-presence numerator/denominator/proportion; prior-comparison fields are typed NA at the first occasion.

`missingness$by_time` fixes variable/time keys, present/usable/missing/conflicting counts, and explicit numerator/denominator/proportion triplets. `entity_summary` fixes never observed, at least once, complete and incomplete among present; `usable_measurement_distribution` covers every integer from zero through the declared time count; `interior_missing` counts entities with a present missing cell strictly between usable cells.

`change$first_to_last` fixes variable keys, presence/single-occasion/present-both/eligible and missing/conflict/nonfinite exclusions, followed by `delta_n`, mean, sample SD, type-7 quartiles, median, min/max/IQR, status and reason. `adjacent` prepends the declared left/right time keys, omits first/last-only presence fields, and otherwise uses the same eligibility and delta contract. Stable zero-eligible rows use `status = "available"`, `reason = "zero_eligible"`.

`issues` columns are `issue_code`, `severity`, time keys, variable keys, `n_affected`, `message`. Code order is missing ID, blank ID, missing time, duplicate ID-time, conflicting variable cell, zero-observation timepoint, then time and variable order. Findings remain value-free warnings with no scientific interpretation.

The `summaries` component is literally identical to `epi_eda_profile_stratified(data, summary_spec, strata = time, include_overall = TRUE, include_missing_stratum = TRUE)` for the same backend. `summary_spec` consists of `time` first and resolved variables in their resolved order. ID and non-selected variables are absent. PostgreSQL uses the canonical inside-transaction helpers rather than nesting the public transaction.

For PostgreSQL, validation, custom aggregates and canonical summaries share one caller-owned read-only repeatable-read snapshot. Identifier grouping is inside PostgreSQL only; no ID, row history or row-bearing value leaves it. Validate source before and inside the snapshot, create no objects, roll back atomically on any failure, sanitise errors and leave the caller connection open and idle.
