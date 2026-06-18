# episcout News

## Development version

- Added `epi_sec_pseudonym()` to create secure participant pseudonymisation
  bridge tables using cryptographic random tokens.
- Added a specification-first EDA workflow driven by data dictionaries:
  `epi_eda_spec()`, `epi_eda_check_schema()`, `epi_eda_profile_missing()`,
  `epi_eda_profile_summaries()`, `epi_eda_profile_plots()`,
  `epi_eda_generate_synthetic_data()`, `epi_eda_run()`, and
  `epi_eda_render_report()`.
- Added `epi_eda_create_project()` and a reusable project scaffold under
  `inst/project-template/`.
- Added fixture-backed tests for the EDA workflow using a pinned
  `blood_storage` fixture.
- Added an HTML EDA report template.
- EDA missingness and summaries now apply specification `missing_codes`.
- Categorical EDA summaries now include `p_observed` alongside the existing
  total-row denominator `p`.
- Documented current MVP limits: basic summaries and plots, synthetic data for
  pipeline preparation only, and no Arrow, DuckDB or data.table EDA backends yet.
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
