# episcout News

## Development version

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
