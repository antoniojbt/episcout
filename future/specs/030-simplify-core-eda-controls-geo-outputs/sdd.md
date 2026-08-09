# Software Design

Spec ID: `030-simplify-core-eda-controls-geo-outputs`
Status: Active

## Public Runner Contract

Keep the existing entry points and add the following arguments after their current behavioural arguments:

```r
maps = FALSE
map_vars = character()
max_map_points = 10000L
```

`epi_eda_run()`, `epi_eda_intake_run()`, `epi_eda_db_run()` and `epi_eda_render_report()` accept the options. `maps` is one non-missing logical. `map_vars` is a character vector of unique exact specification names. `max_map_points` is one positive whole number below the R integer limit and is inclusive. `map_vars` with `maps = FALSE` is an argument error.

Selectors must be declared as numeric, integer, categorical, binary or text. Date and datetime themes are unsupported. Roles never authorise or suppress a selector. Validation occurs before analysis, staging or PostgreSQL transaction work.

The three runners that return workflow objects add `maps` and `map_inventory`. `maps` is a named list containing only created `ggplot` objects. The inventory columns are exact:

```text
map_id, geo_pair, value, status, reason,
n_source_rows, n_mapped, path
```

Rows follow coordinate-pair specification order. For each pair, the geometry map precedes thematic maps in `map_vars` order. Safe identifiers use ordinal metadata rather than filenames derived from caller text: `map-p001-geometry`, `map-p001-v003`; created bundle paths are `maps/<map_id>.svg`. `value` is blank for geometry maps. A not-created row has `n_mapped = 0` and blank `path`. `maps = FALSE` returns an empty list and a typed zero-row inventory.

## Semantic Specification And Dictionary Contract

`epi_eda_spec_scaffold()` returns exactly:

```text
name, label, type, role, units, levels, min, max,
missing_codes, required, group, description,
geo_role, geo_pair, geo_crs
```

Types follow storage only. Factor and fixed logical levels remain storage metadata. All roles, units, bounds, missing codes, groups, descriptions and geo fields are blank or unset. Observed counts, candidate fields, review state/reason and `max_candidate_levels` are removed.

`epi_eda_intake_run(spec = NULL)` generates this dictionary and continues. The returned specification state is factual (`generated` or `supplied`); overall status is only `complete`, `audit_complete` or `blocked`. `review_required`, `caller_asserted`, review guides, report banners and review gates are removed.

The extended PostgreSQL dictionary removes `privacy_class`, `analytic_action`, `validation_status` and `profile_catalogue`; it adds `geo_role`, `geo_pair` and `geo_crs`. Database-owned source metadata, `catalog_name`, `analytic_order`, `provenance`, semantic fields and `drift_status` remain. `epi_eda_dictionary_spec()` propagates geo fields.

Catalogue metadata contains `catalog_name`, `source_value`, `label`, `display_order`, `is_missing` and `provenance`. `epi_db_catalogue_profile()` accepts a separate exact three-column selector named `columns`, validates unique keys and joins it to active dictionary source metadata. It applies only the existing explicit distinct-value bound; it makes no privacy or approval decision.

Deprecated scaffold evidence, combined dictionary policy fields, catalogue validation state and old manifest/linkage schemas receive concise migration errors. Unrelated additional EDA-specification columns remain outside this breaking-schema detector unless they match a removed contract.

## Ordinary Analysis Behaviour

Preparation and stratification do not inspect `review_status`. Summaries and plots do not exclude identifier or coordinate roles. PostgreSQL summaries and compact plot preparation follow the same rule. Type/storage incompatibilities and unsupported temporal preparation remain factual blockers or skips. Identifier QA and coordinate QA remain additional outputs.

`epi_eda_profile_geo()` remains aggregate pair-level QA. Its counts and EPSG:4326 inclusive ranges do not change. The logical field becomes `map_ready`; statuses become `ready` and `not_ready`; neutral reasons are `all_rows_map_ready`, `no_rows`, `incomplete_pairs`, `non_finite_coordinates` and `declared_crs_range_failure` in stable order.

## Data-frame Mapping

The map planner consumes the validated specification and QA table. A pair is map-ready only when it has at least one row, no missing axis, no non-finite value and no applicable range failure. It must also satisfy `n <= max_map_points`. A failed pair creates no geometry, map object, directory or partial SVG; all requested inventory candidates receive the pair QC reason or `max_map_points_exceeded`.

For a ready pair, construct a private frame containing only its coordinates and requested thematic columns. Convert declared thematic missing codes to typed missing values. Call `epi_geo_from_coords(..., remove = FALSE)` and reconcile its audit against EDA QA. Then call `epi_geo_map()` once without `value` and once for every selected theme. No thematic value is selected automatically or collapsed.

`epi_eda_run(output_dir = NULL)` returns objects and deterministic prospective paths but writes no file. With an output directory it writes created SVGs under `maps/`. Intake stages SVGs with all other artifacts. `epi_eda_render_report()` passes through map options and embeds returned map objects. The intake HTML report embeds staged SVGs and displays the inventory, including empty/skipped states.

## PostgreSQL Mapping

The existing read-only repeatable-read transaction remains the only observation boundary. The sequence is:

1. Validate map arguments against the specification and source catalogue before staging.
2. Run row count, schema, missingness and pair QA inside the snapshot.
3. Determine ready pairs and the inclusive bound from snapshot counts.
4. If no pair is ready, maps are disabled or `n > max_map_points`, perform no observation query.
5. Otherwise issue one minimal quoted projection containing the deduplicated coordinate columns for ready pairs and explicit thematic columns only. Normalise declared missing values through existing typed SQL contracts.
6. Apply a defensive `max_map_points + 1` limit, require exact reconciliation with the snapshot count and fail rather than truncate if it changes.
7. Render after the transaction from the bounded private frame and discard it after map creation.

The timing table records `map_collection`, returned rows and the bound without SQL, parameters or values. Failed QC for one pair does not collect its coordinate columns unless an explicitly selected theme or another ready pair independently requires the same column. No map observations are collected at all when `maps = FALSE`.

Map settings are included in run metadata and overwrite identity. The database bundle contract version increments. PostgreSQL report rendering is not added.

## Publication And Manifest Contract

Core EDA manifests contain exactly:

```text
artifact, type, path, status, checksum_md5
```

Dynamic created maps are registered before manifest reconciliation. Every created SVG is a regular file below `maps/`, receives an MD5 checksum and is included in exact staged-content checks. Existing same-directory staging, symlink rejection, atomic swap, rollback and unchanged-owned-bundle overwrite rules remain.

An old sensitivity-bearing manifest is never accepted for overwrite and receives migration guidance. Specialised `epi_sec_*` result manifests are unchanged.

## Security Policy Separation

`epi_sec_linkage_scaffold()` returns `tables`, `columns`, `record_keys` and `crosswalks`. `columns` contains exactly:

```text
source_schema, source_table, source_column,
privacy_class, analytic_action, validation_status
```

It covers every active semantic dictionary row for selected tables, initially using `unclassified`, `review` and `unreviewed`. `epi_sec_linkage_spec()` requires the component, normalises it and validates unique/complete source-key coverage, allowed policy values, confirmed state, exactly one direct-identifier bridge matching each table ID, dropped additional direct identifiers and retained record keys.

`epi_sec_pseudonymise_db()` joins semantic dictionary metadata and linkage column policy by the exact three keys. Policy selects bridge/drop/retain behaviour; the semantic dictionary supplies technical/storage and EDA metadata. Current drift, catalogue, database type, collation, identity, crosswalk, duplicate, lock, transaction, rollback, access and value-free error safeguards remain.

Output dictionaries contain only the new semantic extended dictionary. The generated token row is technical/semantic (`type = text`, `role = id`, generated provenance) but has no privacy/action/validation fields. Output catalogues use the new ordinary catalogue schema and pass directly to EDA.

## Ownership Contract

Core EDA documentation and reports use one statement: episcout creates the outputs explicitly requested by the analyst and does not decide whether they may be shared. Specialised security documentation retains accurate restricted-data and pseudonymisation safeguards without projecting those policies into ordinary EDA.

## Dependencies And Recovery

No dependency is added. Mapping continues to require suggested `sf` and `ggplot2`; PostgreSQL mapping also requires suggested `RPostgres`. `maps = FALSE` does not require map rendering. Any failure during staging, rendering or publication preserves the prior owned bundle. Any security apply failure rolls back database changes under the existing contract.
