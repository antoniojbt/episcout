# Software Design

Spec ID: `029-eda-reviewed-coordinate-roles`
Status: Review

## Scope And Compatibility

Coordinate meaning is optional reviewed metadata and remains orthogonal to `numeric`, `integer`, `categorical`, `binary`, `date`, `datetime` and `text`. A specification that omits all coordinate fields keeps its current validation and analytical semantics. A scaffold adds the three fields as blank character columns and never proposes values from names, observations, ranges, uniqueness or correlations.

The implementation adds one exported profiler:

```r
epi_eda_profile_geo(data, spec)
```

`data` is either a data frame or an `epi_eda_postgres_source`. The function returns one ordinary data frame row per reviewed pair, in first-pair specification order. It returns a typed zero-row result when no coordinate fields are present or every coordinate field is blank.

`epi_eda_run()`, `epi_eda_intake_run()` and `epi_eda_db_run()` add a fixed `geo` result component. Completed bundles add `geo_qa.csv`, classified for disclosure review even though it contains aggregates only. Existing summary, plot, schema, missingness, identifier and Table-1 component schemas and values remain unchanged.

## Specification Contract

The optional fields are all-or-none at the column level:

| Field | Normalized contract |
| --- | --- |
| `geo_role` | Blank, `x` or `y`; trimmed and lower-cased. |
| `geo_pair` | Blank or one non-empty trimmed reviewed identifier. |
| `geo_crs` | Blank or one trimmed EPSG/authority/WKT value accepted by the existing `epi_geo_crs()` resolver. |

A blank `geo_role` requires blank `geo_pair` and `geo_crs`. A non-blank role requires a declared EDA type of `numeric` or `integer`, a non-blank pair and CRS, exactly two rows in the pair, exactly one `x` and one `y`, and the exact same normalized CRS text on both rows. That CRS must resolve successfully through `sf`; the optional dependency is required only when reviewed pairs exist. Variable names remain exact and unique under the existing specification contract.

Any one or two coordinate columns appearing without the complete three-field set is rejected. Validation errors may name specification fields, roles, pair identifiers and variable names, but never source observations or coordinates.

## Aggregate QA Contract

The result columns are fixed:

```text
geo_pair, x_name, y_name, geo_crs, crs_epsg,
n, complete_pairs, missing_x, missing_y, both_missing,
non_finite, range_failures, eligible, status, reason
```

Character, integer and logical types are stable even for zero rows. `geo_pair`, variable names and CRS are caller-authored reviewed specification metadata. `crs_epsg` is the resolved EPSG integer when available and typed `NA` otherwise.

Missingness uses each variable's existing reviewed `missing_codes` contract in addition to ordinary missing values. `NaN`, positive infinity and negative infinity are non-finite rather than ordinary missing unless an existing input contract already refuses them. Counts mean:

- `n`: all source rows in the reviewed snapshot or data frame;
- `complete_pairs`: rows where both coordinates are finite and not reviewed-missing, including finite rows that later fail range review;
- `missing_x`/`missing_y`: rows where exactly that axis is reviewed-missing;
- `both_missing`: rows where both axes are reviewed-missing;
- `non_finite`: rows where either non-missing axis is `NaN` or infinite;
- `range_failures`: finite complete EPSG:4326 rows outside inclusive x `[-180, 180]` or y `[-90, 90]`; zero for other resolved CRS contracts because episcout has no generic reviewed area-of-use policy.

The row is `eligible = TRUE`, `status = "eligible"`, `reason = "all_rows_eligible"` only when `n > 0` and every blocker count is zero. A zero-row source is `status = "not_eligible"`, `reason = "no_rows"`. Other ineligible reasons are a stable `;`-joined sequence drawn from `incomplete_pairs`, `non_finite_coordinates` and `reviewed_crs_range_failure`. Counts, rather than the reason string, remain the quantitative authority.

Eligibility means only that the reviewed pair can be passed separately to `epi_geo_from_coords()` after privacy and scientific review. It is not geometry construction, disclosure approval or confirmation that coordinates represent the intended place, person, time or unit.

## Data-frame Path

1. Parse and validate the specification before reading pair values.
2. Validate that every reviewed x/y variable exists and has numeric or integer storage compatible with the specification.
3. For each pair, build private missing masks through existing EDA missing-code helpers.
4. Calculate only the fixed aggregate counts, discard all row masks and values, and return rows in specification order.
5. Leave the caller's data and specification objects unchanged.

The profiler never calls `sf::st_as_sf()`, creates geometry, calculates bounds or returns row indices. The CRS resolver is used for metadata validation only.

## PostgreSQL Path

The PostgreSQL profiler validates the caller-owned source and executes inside the existing owned `REPEATABLE READ READ ONLY` transaction when called by `epi_eda_db_run()`. Direct profiling owns an equivalent transaction. Each pair issues one aggregate query that returns exactly one row and uses quoted identifiers plus the existing reviewed missing-code parameter contracts.

The query computes counts with `count(*) FILTER (...)`. Numeric special values are classified inside SQL without selecting coordinate columns. EPSG:4326 range constants are bound parameters. The implementation records one `geo_pair_qa` timing row per pair with `rows_returned = 1` and never calls PostGIS functions, checks for a PostGIS extension, materialises WKT/WKB or fetches coordinate observations.

PostgreSQL column presence/type compatibility and catalogue drift fail before pair QA. An error remains value-free and leaves the caller-owned connection open, idle and usable.

## Workflow And Bundle Integration

- `epi_eda_run()` computes `geo` after missingness and before type summaries; `write_run_eda_outputs()` writes `geo_qa.csv`.
- `epi_eda_intake_run()` computes pair QA from the private prepared analysis frame after preparation and before canonical completion. Its fixed result and manifest gain `geo`; the report may display the aggregate table with the scientific/privacy warning.
- `epi_eda_db_run()` computes pair QA in its single snapshot, reconciles pair counts against `n_total`, writes `geo_qa.csv`, includes it in the manifest and returns it as `geo`.
- The caller-authored reviewed specification remains in bundles under its existing sensitivity classification. No new file contains source values, row identifiers, geometry or bounds.

Reconciliation checks each count is a non-negative integer not exceeding `n`, `n` equals the workflow denominator, missing partitions reconcile, `complete_pairs` does not exceed the remaining rows and eligibility/status/reason agree with counts. Overwrite validation continues to require an exact owned manifest and checksum set; the new artifact is part of the new fixed bundle contract.

## Edge Cases

- No geo fields, all blank fields and zero-column scaffolds return the typed empty component.
- Zero rows produce one not-eligible row per reviewed pair with all counts zero.
- Non-syntactic and Unicode variable/pair identifiers remain exact.
- Multiple independent pairs retain first appearance order.
- Reviewed sentinels can produce missing-x, missing-y or both-missing counts without entering output.
- One row can contribute to a missing-axis count and `non_finite` when the other axis is non-finite; all counts remain explicit and bounded rather than falsely partitioned.
- EPSG:4326 boundary values are accepted; other resolved CRSs do not receive invented global limits.
- Identifier role and coordinate role are separate metadata. Existing identifier exclusion remains authoritative for ordinary summaries and does not suppress aggregate pair QA.

## Dependencies And Recovery

No dependency is added. Reuse optional `sf` for CRS resolution, imported `DBI`, suggested `RPostgres` and existing transaction/query helpers. PostGIS is not required.

Implementation remains isolated on `feature/eda-reviewed-coordinate-roles`, stacked on the green planning PR under the owner's explicit instruction. If value-free parity, fixed bundle publication or compatibility cannot satisfy this contract, stop and record the conflict in `review.md`; do not weaken the privacy boundary or infer coordinate semantics.
