# episcout News

## Development version

- Added `epi_eda_approved_rules()` and `epi_eda_apply_cleaning_rules()` for explicit analyst-approved bounds, allowed values and missing codes. The exact opaque-keyed rule schema is separate from pending QC proposals and descriptive dictionary metadata; data-frame processing returns a complete processed result with optional no-replace CSV/RDS publication, while PostgreSQL creates one new transactionally reconciled table through server-side transformations. Both paths preserve the source and return only aggregate missingness transitions, dimensions and a deterministic rule hash in the audit.
- Added `epi_eda_qc_proposals()` for deterministic aggregate QC evidence and separate pending review prompts from in-memory or PostgreSQL sources. Caller-managed opaque keys link the tables without returning source names; exact observed 0/1 and Tukey 1.5-IQR signals remain review-only, units and missing codes are never inferred, and the semantic dictionary and source data remain unchanged.
- Added `epi_eda_categorical_display()` as the shared categorical numerator, denominator and proportion contract for canonical and stratified EDA. Its explicit compatibility, column, row and overall bases now drive Table 1 and aggregate frequency companions; standard and declared-code missing values share one visible level, zero denominators remain zero with unavailable proportions, delivery reports show the companions, and valid legacy four-column delivery inputs remain renderable.
- Added a canonical PostgreSQL EDA delivery layout and `epi_eda_render_db_report()`. Delivery runs publish one owned README/HTML entry point, aggregate QA, compact plot data, SVGs and run manifests after the read-only snapshot closes. Existing flat bundles can be rendered later without a connection; manifest ownership, checksums, relative paths, relocation and atomic replacement are validated before publication. The default flat bundle and in-memory `epi_eda_render_report()` contracts remain compatible.
- Simplified core EDA to use lean semantic dictionaries without approval states, privacy/action policy, role-based output suppression or sensitivity-bearing manifests. Intake-generated dictionaries now continue through factual processing, ordinary catalogues use explicit three-key profiling selectors, and old schemas fail with migration guidance. episcout creates the outputs explicitly requested by the analyst and does not decide whether they may be shared.
- Added opt-in bounded coordinate-derived point maps to `epi_eda_run()`, `epi_eda_intake_run()`, `epi_eda_db_run()` and `epi_eda_render_report()`. Geometry and explicit thematic maps have deterministic inventories and SVG paths; failed pair QA and sources over `max_map_points` are recorded without partial output or truncation. PostgreSQL collects only requested ready-pair coordinates and themes inside the profiling snapshot.
- Kept `geo_role`, `geo_pair` and `geo_crs` as explicit technical semantics and retained `epi_eda_profile_geo()` as pair-level map-readiness QA. Identifier and coordinate roles no longer suppress their declared summaries, plots or stratification.
- Moved pseudonymisation column policy into the four-component `epi_sec_linkage_*` contract. Semantic output dictionaries and catalogues now pass directly into EDA, while restricted security manifests, registry, audit/apply, crosswalk and rollback safeguards remain unchanged.
- Added a read-only PostGIS geospatial workflow through `epi_geo_postgis_source()`, `epi_geo_postgis_describe()` and `epi_geo_postgis_collect()`. It uses caller-owned RPostgres connections, exact quoted relation/column identifiers, aggregate-only repeatable-read QA and an explicit fail-not-truncate feature bound; feature collection also requires an ordinary-column allow-list and accepts only a typed bbox in the source CRS. The workflow performs no writes, DDL, extension installation, arbitrary SQL or automatic collection.
- Added an optional `sf`-based vector-mapping foundation through `epi_geo_read()`, `epi_geo_from_coords()`, `epi_geo_describe()`, `epi_geo_transform()`, `epi_geo_map()` and `epi_geo_write()`. The offline guide covers explicit coordinate meaning, GeoPackage/Shapefile boundaries, aggregate geometry QA, true CRS transformation, extensible static maps and staged exact-layer GeoPackage publication; inference, raster, web maps and coordinate inference remain deferred.
- Added `epi_clean_curp_audit()` for vector-safe, privacy-aware local CURP structure checks and exact reconciliation with reviewed birth-date, recorded-sex, birthplace-code and initials references. The result omits the supplied CURP, reports value-free issues and aggregate-only printing, distinguishes missing and unavailable comparisons, and explicitly reports checksum verification as `not_verified`; it does not claim registry assignment, certification or identity. The legacy `epi_clean_curp()` now honours its documented vector interface while retaining its 13-column compatibility schema.
- Reused the transaction-local PostgreSQL EDA relation count in categorical and binary summaries, removing one redundant `COUNT(*)` query per affected variable without changing aggregates, public interfaces or bundle output.
- Added a value-free, audit-first PostgreSQL identifier-universe workflow for one reviewed namespace distributed across multiple ordinary tables. It reconciles source quality, distinct-universe membership and pairwise overlap in a read-only snapshot, then can atomically publish a blocker-free restricted canonical universe without generating pseudonyms or collecting identifiers into ordinary R results.

## 0.3.0

- Corrected the pre-release `epi_db_catalogue_profile()` result contract so
  non-missing value counts are returned in `values` and one aggregate
  PostgreSQL NULL count per profiled column is returned in `missing`. Empty and
  all-NULL tables retain a missing-count row, `values$source_value` is never
  missing, and `max_levels` now unambiguously bounds only non-missing values.
- Corrected `epi_write_df()` so `suffix = "csv"` writes comma-separated bytes
  instead of tab-separated content with a `.csv` filename. The function now
  accepts only CSV or TSV suffixes, infers their delimiter, rejects explicit
  suffix/delimiter contradictions and requires an existing output directory.
  Existing TSV output is unchanged; callers that relied on the mislabeled CSV
  bytes must request `"tsv"` instead.
- Added a deterministic PostgreSQL EDA scalability gate covering one million rows, complete SVG bundle publication and bounded client collection, and strengthened database condition handling so native server notices and warnings are re-signalled with fixed value-free text.
- Fixed PostgreSQL pseudonymisation apply so session advisory locks are released exactly once when transaction-scoped protection takes over, eliminating successful-run lock-ownership warnings while preserving timeout and rollback cleanup.
- Added a runnable database-to-report walkthrough with a neutral longitudinal CSV fixture. The commented R script demonstrates duplicate review, PostgreSQL inventory and dictionaries, stable pseudonymisation, aggregate-only database EDA, plots, Table 1 and HTML report output without embedding credentials.
- Added PostgreSQL 17-backed specification-first EDA through `epi_eda_postgres_source()` and `epi_eda_db_run()`, with read-only repeatable-read snapshots, aggregate-only canonical profiling, explicit identifier QA, bounded compact plot data, redacted source display, deterministic SVG paths and manifest-owned staged bundle publication. Ordinary data-frame interfaces and the six summary components remain unchanged; text plots now show character lengths and explicit identifier roles produce no value-bearing summary or plot.
- Made local datetime preparation deterministic across supported platforms by classifying and converting wall times with `clock`'s packaged IANA timezone data. Ambiguous, nonexistent and unsupported local times block with value-free guidance; this raises the minimum supported R version to 4.0.
- Added a generic, audit-first PostgreSQL longitudinal pseudonymisation workflow with value-free linkage metadata, a stable restricted identity registry, exact reviewed crosswalks, explicit longitudinal duplicate handling, atomic output writes and a dedicated first-time-user guide. Pseudonymised outputs remain restricted personal data and are not anonymous or automatically disclosure-controlled.
- Added `epi_eda_intake_run()` as a stage-gated new-dataset workflow that creates a review-required specification scaffold, audits or applies reviewed preparation, writes reconciled canonical and optional stratified summaries, and renders a portable aggregate-only HTML bundle with a fixed manifest and safe owned-file collision handling.
- Added specification-aware grouped descriptive summaries and a separate traceable long-form Table 1 renderer, preserving declared empty groups/levels, explicit denominators, missing/unexpected strata, canonical Overall semantics and aggregate-only text diagnostics without p-values or implicit disclosure control.
- Added `epi_eda_prepare()` for privacy-conscious audit and all-or-nothing application of reviewed missing-sentinel, type, categorical-level and strict temporal preparation rules, with stable machine-readable audits and before/after schemas.
- Added `epi_eda_spec_scaffold()` to create a privacy-conscious, human-review EDA specification draft from an existing data frame using storage classes and aggregate evidence without enumerating observed candidate values.
- Corrected event proportions so numerator and denominator use the same requested analysis window, with explicit errors for absent columns, empty eligible populations and missing or non-binary eligible outcomes.
- Made `epi_stats_numeric(na.rm = FALSE)` preserve factual counts while returning unavailable analytical results instead of silently excluding missing values.
- Corrected transpose labels for arbitrary identifier-column positions, made repeated-measure spreading safe for zero, nonconsecutive and unbalanced visits, and made nested data-frame joins full outer by default while retaining explicit legacy left joins with `all.y = FALSE`.
- Excluded specification missing sentinels from EDA plot layers and added descriptive `type_status` and `type_reason` fields to schema output without changing the historical presence fields.
- Fixed synthetic integer generation for singleton bounds and intervals that contain no integer values.
- Replaced the overlapping EDA summary interfaces with one canonical six-component contract covering numeric, integer, categorical, binary, text, date and datetime variables, with explicit variable status, skipped reasons and machine-readable output tables. The unreleased summary selector and legacy two-table adapter were removed.
- Aligned active univariate `epi_stats_*` and EDA summaries on shared internal type cores, added typed `epi_stats_summary()` output, made absent-variable counts unavailable rather than inferred, ignored undeclared factor metadata, and made all-missing or non-finite numeric totals unavailable instead of zero.

## 0.2.0

- Added `epi_sec_pseudonym()` to create secure participant pseudonymisation bridge tables using cryptographic random tokens.
- Added a specification-first EDA workflow driven by data dictionaries: `epi_eda_spec()`, `epi_eda_check_schema()`, `epi_eda_profile_missing()`, `epi_eda_profile_summaries()`, `epi_eda_profile_plots()`, `epi_eda_generate_synthetic_data()`, `epi_eda_run()`, and `epi_eda_render_report()`.
- Added `epi_eda_create_project()` and a reusable project scaffold under `inst/project-template/`.
- Added fixture-backed tests for the EDA workflow using a pinned `blood_storage` fixture.
- Added an HTML EDA report template.
- EDA missingness and summaries now apply specification `missing_codes`.
- Categorical EDA summaries now include `p_observed` alongside the existing total-row denominator `p`.
- Documented current MVP limits: basic summaries and plots, synthetic data for pipeline preparation only, and no Arrow, DuckDB or data.table EDA backends yet.
- Cleaned up Rd documentation and CRAN-check issues on the development branch.

## 0.1.4

- Added `epi_plot_theme_imss` and colour palette helpers.
- Added `epi_plot_add_var_labels`.
- Reworked `epi_stats_*` summary functions.

## 0.1.3

- Improved coverage tests.
- Added helper wrappers.
- Improved documentation.

## 0.1.2

- Minor bug fixes and internal improvements.

## 0.1.1

- First release.
